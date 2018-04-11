/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.personalincome.controllers

import javax.inject.{Inject, Named, Singleton}
import play.api._
import play.api.http.HeaderNames
import play.api.libs.json.Json.toJson
import play.api.libs.json.{JsError, Json}
import play.api.mvc.{BodyParsers, Request, Result}
import uk.gov.hmrc.api.controllers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.{HeaderCarrier, NotFoundException, ServiceUnavailableException}
import uk.gov.hmrc.personalincome.connectors.Error
import uk.gov.hmrc.personalincome.controllers.HeaderKeys.tcrAuthToken
import uk.gov.hmrc.personalincome.controllers.action.AccessControl
import uk.gov.hmrc.personalincome.domain.{RenewalReference, _}
import uk.gov.hmrc.personalincome.services.{LivePersonalIncomeService, PersonalIncomeService, SandboxPersonalIncomeService}
import uk.gov.hmrc.play.HeaderCarrierConverter.fromHeadersAndSession
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.collection.Seq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

object SummaryFormat extends Enumeration {
  type SummaryFormat = Value
  val Classic, Refresh = Value

  def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
}

trait ErrorHandling {
  self: BaseController =>

  def notFound = Status(ErrorNotFound.httpStatusCode)(toJson(ErrorNotFound))

  def errorWrapper(func: => Future[mvc.Result])(implicit hc: HeaderCarrier) = {
    func.recover {
      case ex: NotFoundException => notFound

      case ex: ServiceUnavailableException =>
        // The hod can return a 503 HTTP status which is translated to a 429 response code.
        // The 503 HTTP status code must only be returned from the API gateway and not from downstream API's.
        Logger.error(s"ServiceUnavailableException reported: ${ex.getMessage}", ex)
        Status(ClientRetryRequest.httpStatusCode)(toJson(ClientRetryRequest))

      case e: Throwable =>
        Logger.error(s"Internal server error: ${e.getMessage}", e)
        Status(ErrorInternalServerError.httpStatusCode)(toJson(ErrorInternalServerError))
    }
  }
}

trait PersonalIncomeController extends BaseController with AccessControl with ErrorHandling with ConfigLoad {

  val service: PersonalIncomeService
  val taxCreditsSubmissionControlConfig: TaxCreditsControl
  val logger: LoggerLike

  def addCacheHeader(maxAge:Long, result:Result):Result = {
    result.withHeaders(HeaderNames.CACHE_CONTROL -> s"max-age=$maxAge")
  }

  final def getSummary(nino: Nino, year: Int, journeyId: Option[String] = None) = validateAcceptWithAuth(acceptHeaderValidationRules, Option(nino)).async {
    implicit request =>
    implicit val hc = fromHeadersAndSession(request.headers, None)
    errorWrapper(service.getTaxSummary(nino, year, journeyId).map {
      case Some(summary) => Ok(toJson(summary))
      case _ => NotFound
    })
  }

  final def getRenewalAuthentication(nino: Nino, renewalReference: RenewalReference, journeyId: Option[String] = None) =
    validateAcceptWithAuth(acceptHeaderValidationRules, Option(nino)).async {
      implicit request =>
        implicit val hc = fromHeadersAndSession(request.headers, None)
        errorWrapper(
          service.authenticateRenewal(nino, renewalReference).map {
            case Some(authToken) => Ok(toJson(authToken))
            case _ => notFound
          })
  }

  final def getTaxCreditExclusion(nino: Nino, journeyId: Option[String] = None) = validateAcceptWithAuth(acceptHeaderValidationRules, Option(nino)).async {
    implicit request =>
      implicit val hc = fromHeadersAndSession(request.headers, None)
      errorWrapper(
        service.getTaxCreditExclusion(nino).map { res => Ok(Json.parse(s"""{"showData":${!res.excluded}}""")) })
  }

  def addMainApplicantFlag(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Result] = {
    service.claimantDetails(nino).map { claim =>
      val mainApplicantFlag: String = if (claim.mainApplicantNino == nino.value) "true" else "false"
      Ok(toJson(claim.copy(mainApplicantNino = mainApplicantFlag)))
    }
  }

  final def claimantDetails(nino: Nino, journeyId: Option[String] = None, claims: Option[String] = None) =
    validateAcceptWithAuth(acceptHeaderValidationRules, Option(nino)).async {
      implicit request =>
        implicit val hc = fromHeadersAndSession(request.headers, None)

        errorWrapper(validateTcrAuthHeader(claims) {
          implicit hc =>
            def singleClaim: Future[Result] = addMainApplicantFlag(nino)

            def retrieveAllClaims = service.claimantClaims(nino).map { claims =>
              claims.references.fold(notFound) { found => if (found.isEmpty) notFound else Ok(toJson(claims)) }
            }

            claims.fold(singleClaim) { _ => retrieveAllClaims.map(addCacheHeader(maxAgeForClaims, _)) }
        })
  }

  final def fullClaimantDetails(nino: Nino, journeyId: Option[String] = None) =
    validateAcceptWithAuth(acceptHeaderValidationRules, Option(nino)).async {
      implicit request =>
        implicit val hc = fromHeadersAndSession(request.headers, None)

        errorWrapper({
          def fullClaimantDetails(claim: Claim): Future[Claim] = {
            def getClaimantDetail(token:TcrAuthenticationToken, hc: HeaderCarrier) = {
              implicit val hcWithToken: HeaderCarrier = hc.copy(extraHeaders = Seq(tcrAuthToken -> token.tcrAuthToken))

              service.claimantDetails(nino).map { claimantDetails =>
                claim.copy(
                  household = claim.household.copy(),
                  renewal = claim.renewal.copy(renewalFormType = Some(claimantDetails.renewalFormType)))
              }
            }

            service.authenticateRenewal(nino, RenewalReference(claim.household.barcodeReference)).flatMap { maybeToken =>
              if (maybeToken.nonEmpty) getClaimantDetail(maybeToken.get,hc)
              else  Future successful claim
            }.recover{
              case e: Exception => {
                logger.warn(s"${e.getMessage} for ${claim.household.barcodeReference}")
                claim
              }
            }
          }

          val eventualClaims: Future[Seq[Claim]] = service.claimantClaims(nino).flatMap { claimantClaims =>
            val claims: Seq[Claim] = claimantClaims.references.getOrElse(Seq.empty[Claim])

            if ( claims.isEmpty ) logger.warn(s"Empty claims list for journeyId $journeyId")

            Future sequence claims.map { claim =>
              val barcodeReference = claim.household.barcodeReference

              if (barcodeReference.equals("000000000000000")) {
                logger.warn(s"Invalid barcode reference ${barcodeReference} for journeyId $journeyId applicationId ${claim.household.applicationID}")
                Future successful claim
              } else fullClaimantDetails(claim)
            }
          }

          eventualClaims.map{ claimantDetails =>
            Ok(toJson(Claims(if (claimantDetails.isEmpty) None else Some(claimantDetails))))
          }
        })
  }

  final def submitRenewal(nino: Nino, journeyId: Option[String] = None) =
    validateAcceptWithAuth(acceptHeaderValidationRules, Option(nino)).async(BodyParsers.parse.json) {
      implicit request =>
        implicit val hc = fromHeadersAndSession(request.headers, None)

        val enabled = taxCreditsSubmissionControlConfig.toTaxCreditsRenewalsState.submissionsState

        request.body.validate[TcrRenewal].fold(
          errors => {
            logger.warn("Received error with service submitRenewal: " + errors)
            Future.successful(BadRequest(Json.obj("message" -> JsError.toJson(errors))))
          },
          renewal => {
            errorWrapper(validateTcrAuthHeader(None) {
              implicit hc =>
                if (enabled != "open") {
                  logger.info("Renewals have been disabled.")
                  Future.successful(Ok)
                } else {
                  service.submitRenewal(nino, renewal).map {
                    case Error(status) => {
                      logger.warn(s"Tax credit renewal submission failed with status $status for journeyId $journeyId")
                      Status(status)(toJson(ErrorwithNtcRenewal))
                    } case _ => {
                      logger.info(s"Tax credit renewal submission successful for journeyId $journeyId")
                      Ok
                    }
                  }.recover{
                    case e: Exception => {
                      logger.warn(s"Tax credit renewal submission failed with exception ${e.getMessage} for journeyId $journeyId")
                      throw e
                    }
                  }
                }
            })
          }
        )
  }

  final def taxCreditsSummary(nino: Nino, journeyId: Option[String] = None) =  validateAcceptWithAuth(acceptHeaderValidationRules, Option(nino)).async {
    implicit request =>
      implicit val hc = fromHeadersAndSession(request.headers, None)
      errorWrapper(service.getTaxCreditSummary(nino).map(summary => Ok(toJson(summary))))
  }

  private def validateTcrAuthHeader(mode:Option[String])(func: HeaderCarrier => Future[mvc.Result])(implicit request: Request[_], hc: HeaderCarrier) = {

    (request.headers.get(tcrAuthToken), mode) match {

      case (None , Some(value)) => func(hc)

      case (Some(token), None) => func(hc.copy(extraHeaders = Seq(tcrAuthToken -> token)))

      case _ =>
        val default: ErrorResponse = ErrorNoAuthToken
        val authTokenShouldNotBeSupplied = ErrorAuthTokenSupplied
        val response = mode.fold(default){ found => authTokenShouldNotBeSupplied}
        logger.warn("Either tcrAuthToken must be supplied as header or 'claims' as query param.")
        Future.successful(Forbidden(toJson(response)))
    }
  }

}

trait ConfigLoad {
  val maxAgeClaimsConfig = "claims.maxAge"
  def getConfigForClaimsMaxAge:Option[Long]

  lazy val maxAgeForClaims: Long = getConfigForClaimsMaxAge
    .getOrElse(throw new Exception(s"Failed to resolve config key $maxAgeClaimsConfig"))
}

@Singleton
class SandboxPersonalIncomeController @Inject()(override val authConnector: AuthConnector,
                                                @Named("controllers.confidenceLevel") override val confLevel: Int,
                                                override val logger: LoggerLike) extends PersonalIncomeController {
  override lazy val requiresAuth: Boolean = false
  override val service = SandboxPersonalIncomeService
  override val taxCreditsSubmissionControlConfig: TaxCreditsControl = new TaxCreditsControl {
    override def toTaxCreditsSubmissions: TaxCreditsSubmissions = new TaxCreditsSubmissions(false, true, true )

    override def toTaxCreditsRenewalsState: TaxCreditsRenewalsState =
      TaxCreditsRenewalsState(submissionsState = "open")
  }
  override def getConfigForClaimsMaxAge = Play.current.configuration.getLong(maxAgeClaimsConfig)
}

@Singleton
class LivePersonalIncomeController @Inject()(override val authConnector: AuthConnector,
                                             @Named("controllers.confidenceLevel") override val confLevel: Int,
                                             override val logger: LoggerLike) extends PersonalIncomeController {
  override val service = LivePersonalIncomeService
  override val taxCreditsSubmissionControlConfig: TaxCreditsControl = TaxCreditsSubmissionControl
  override def getConfigForClaimsMaxAge = Play.current.configuration.getLong(maxAgeClaimsConfig)
}
