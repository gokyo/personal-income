/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api._
import play.api.http.HeaderNames
import play.api.libs.json.{JsError, Json}
import play.api.mvc.{Result, BodyParsers, Request}
import uk.gov.hmrc.api.controllers._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.connectors.Error
import uk.gov.hmrc.personalincome.controllers.action.{AccountAccessControlCheckOff, AccountAccessControlWithHeaderCheck}
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.personalincome.services.{LivePersonalIncomeService, PersonalIncomeService, SandboxPersonalIncomeService}
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

object SummaryFormat extends Enumeration {
  type SummaryFormat = Value
  val Classic, Refresh = Value

  def withNameOpt(s: String): Option[Value] = values.find(_.toString == s)
}

trait ErrorHandling {
  self: BaseController =>

  def notFound = Status(ErrorNotFound.httpStatusCode)(Json.toJson(ErrorNotFound))

  def errorWrapper(func: => Future[mvc.Result])(implicit hc: HeaderCarrier) = {
    func.recover {
      case ex: NotFoundException => notFound

      case ex: ServiceUnavailableException =>
        // The hod can return a 503 HTTP status which is translated to a 429 response code.
        // The 503 HTTP status code must only be returned from the API gateway and not from downstream API's.
        Logger.error(s"ServiceUnavailableException reported: ${ex.getMessage}", ex)
        Status(ClientRetryRequest.httpStatusCode)(Json.toJson(ClientRetryRequest))

      case e: Throwable =>
        Logger.error(s"Internal server error: ${e.getMessage}", e)
        Status(ErrorInternalServerError.httpStatusCode)(Json.toJson(ErrorInternalServerError))
    }
  }
}

trait PersonalIncomeController extends BaseController with HeaderValidator with ErrorHandling with ConfigLoad {

  val service: PersonalIncomeService
  val accessControl: AccountAccessControlWithHeaderCheck
  val taxCreditsSubmissionControlConfig: TaxCreditsControl

  def addCacheHeader(maxAge:Long, result:Result):Result = {
    result.withHeaders(HeaderNames.CACHE_CONTROL -> s"max-age=$maxAge")
  }

  final def getSummary(nino: Nino, year: Int, journeyId: Option[String] = None) = accessControl.validateAcceptWithAuth(acceptHeaderValidationRules, Some(nino)).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(service.getTaxSummary(nino, year, journeyId).map {
        case Some(summary) => Ok(Json.toJson(summary))
        case _ => NotFound
      })
  }

  final def getRenewalAuthentication(nino: Nino, renewalReference: RenewalReference, journeyId: Option[String] = None) = accessControl.validateAcceptWithAuth(acceptHeaderValidationRules, Some(nino)).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(
        service.authenticateRenewal(nino, renewalReference).map {
          case Some(authToken) => Ok(Json.toJson(authToken))
          case _ => notFound
        })
  }

  final def getTaxCreditExclusion(nino: Nino, journeyId: Option[String] = None) = accessControl.validateAcceptWithAuth(acceptHeaderValidationRules, Some(nino)).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(
        service.getTaxCreditExclusion(nino).map { res => Ok(Json.parse(s"""{"showData":${!res.excluded}}""")) })
  }

  def addMainApplicantFlag(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Result] = {
    service.claimantDetails(nino).map { claim =>

      val mainApplicantFlag: String = if (claim.mainApplicantNino == nino.value) "true" else "false"
      Ok(Json.toJson(claim.copy(mainApplicantNino = mainApplicantFlag)))
    }
  }

  final def claimantDetails(nino: Nino, journeyId: Option[String] = None, claims: Option[String] = None) = accessControl.validateAcceptWithAuth(acceptHeaderValidationRules, Some(nino)).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)

      errorWrapper(validateTcrAuthHeader(claims) {
          implicit hc =>
            def singleClaim: Future[Result] = addMainApplicantFlag(nino)

            def retrieveAllClaims = service.claimantClaims(nino).map { claims =>
              claims.references.fold(notFound){found => if (found.isEmpty) notFound else Ok(Json.toJson(claims))}}

            claims.fold(singleClaim){_ => retrieveAllClaims.map(addCacheHeader(maxAgeForClaims, _))}
      })
  }

  final def submitRenewal(nino: Nino, journeyId: Option[String] = None) = accessControl.validateAcceptWithAuth(acceptHeaderValidationRules, Some(nino)).async(BodyParsers.parse.json) {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)

      val enabled = taxCreditsSubmissionControlConfig.toSubmissionState.submissionState

      request.body.validate[TcrRenewal].fold(
        errors => {
          Logger.warn("Received error with service submitRenewal: " + errors)
          Future.successful(BadRequest(Json.obj("message" -> JsError.toJson(errors))))
        },
        renewal => {
          errorWrapper(validateTcrAuthHeader(None) {
              implicit hc =>
                if (!enabled) {
                  Logger.info("Renewals have been disabled.")
                  Future.successful(Ok)
                } else {
                  service.submitRenewal(nino, renewal).map {
                    case Error(status) => Status(status)(Json.toJson(ErrorwithNtcRenewal))
                    case _ => Ok
                  }
                }
          })
        }
      )
  }

  final def taxCreditsSummary(nino: Nino, journeyId: Option[String] = None) = accessControl.validateAcceptWithAuth(acceptHeaderValidationRules, Some(nino)).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper {
        service.getTaxCreditSummary(nino).map { summary =>
          summary match {
            case Left(_) => Ok(Json.toJson(summary.left.get))
            case Right(_) => Ok(Json.toJson(summary.right.get))
          }
        }
      }
  }

  private def validateTcrAuthHeader(mode:Option[String])(func: HeaderCarrier => Future[mvc.Result])(implicit request: Request[_], hc: HeaderCarrier) = {

    (request.headers.get(HeaderKeys.tcrAuthToken), mode) match {

      case (None , Some(value)) => func(hc)

      case (t@Some(token), None) => func(hc.copy(extraHeaders = Seq(HeaderKeys.tcrAuthToken -> token)))

      case _ =>
        val default: ErrorResponse = ErrorNoAuthToken
        val authTokenShouldNotBeSupplied = ErrorAuthTokenSupplied
        val response = mode.fold(default){ found => authTokenShouldNotBeSupplied}
        Logger.warn("Either tcrAuthToken must be supplied as header or 'claims' as query param.")
        Future.successful(Forbidden(Json.toJson(response)))
    }
  }

}

trait ConfigLoad {
  val maxAgeClaimsConfig = "claims.maxAge"
  def getConfigForClaimsMaxAge:Option[Long]

  lazy val maxAgeForClaims: Long = getConfigForClaimsMaxAge
    .getOrElse(throw new Exception(s"Failed to resolve config key $maxAgeClaimsConfig"))
}


object SandboxPersonalIncomeController extends PersonalIncomeController {
  override val service = SandboxPersonalIncomeService
  override val accessControl = AccountAccessControlCheckOff
  override val taxCreditsSubmissionControlConfig: TaxCreditsControl = new TaxCreditsControl {
    override def toTaxCreditsSubmissions: TaxCreditsSubmissions = new TaxCreditsSubmissions(false, true)

    override def toSubmissionState: SubmissionState = SubmissionState(submissionState = true)
  }
  override def getConfigForClaimsMaxAge = Play.current.configuration.getLong(maxAgeClaimsConfig)
}

object LivePersonalIncomeController extends PersonalIncomeController {
  override val service = LivePersonalIncomeService
  override val accessControl = AccountAccessControlWithHeaderCheck
  override val taxCreditsSubmissionControlConfig: TaxCreditsControl = TaxCreditsSubmissionControl
  override def getConfigForClaimsMaxAge = Play.current.configuration.getLong(maxAgeClaimsConfig)
}
