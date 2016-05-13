/*
 * Copyright 2016 HM Revenue & Customs
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

import play.api.mvc.{Request, BodyParsers}
import uk.gov.hmrc.personalincome.connectors.Error
import uk.gov.hmrc.personalincome.controllers.action.{AccountAccessControlWithHeaderCheck, AccountAccessControlCheckAccessOff}
import play.api.libs.json.{JsError, Json}
import uk.gov.hmrc.personalincome.domain.{TcrRenewal, RenewalReference}
import uk.gov.hmrc.personalincome.services.{LivePersonalIncomeService, PersonalIncomeService, SandboxPersonalIncomeService}
import uk.gov.hmrc.domain.Nino
import play.api.{mvc, Logger}
import uk.gov.hmrc.play.http.{HeaderCarrier, NotFoundException}
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.api.controllers._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ErrorHandling {
  self: BaseController =>

  def errorWrapper(func: => Future[mvc.Result])(implicit hc: HeaderCarrier) = {
    func.recover {
      case ex: NotFoundException => Status(ErrorNotFound.httpStatusCode)(Json.toJson(ErrorNotFound))

      case e: Throwable =>
        Logger.error(s"Internal server error: ${e.getMessage}", e)
        Status(ErrorInternalServerError.httpStatusCode)(Json.toJson(ErrorInternalServerError))
    }
  }
}

trait PersonalIncomeController extends BaseController with HeaderValidator with ErrorHandling {

  val service: PersonalIncomeService
  val accessControl:AccountAccessControlWithHeaderCheck

  final def getSummary(nino: Nino, year: Int) = accessControl.validateAccept(acceptHeaderValidationRules).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(service.getSummary(nino, year).map(as => Ok(Json.toJson(as))))
  }

  final def getRenewalAuthentication(nino: Nino, renewalReference:RenewalReference) = accessControl.validateAccept(acceptHeaderValidationRules).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(
        service.authenticateRenewal(nino, renewalReference).map {
          case Some(authToken) => Ok(Json.toJson(authToken))
          case _ => InternalServerError(Json.toJson(ErrorwithNtcRenewalAuthentication))
      })
  }

  final def claimantDetails(nino: Nino) = accessControl.validateAccept(acceptHeaderValidationRules).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(validateTcrAuthHeader() {
        token =>
          implicit hc =>
            service.claimantDetails(nino).map(as => Ok(Json.toJson(as)))
      })
  }

  final def submitRenewal(nino: Nino) = accessControl.validateAccept(acceptHeaderValidationRules).async(BodyParsers.parse.json) {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)

      request.body.validate[TcrRenewal].fold (
        errors => {
          Logger.warn("Received error with service submitRenewal: " + errors)
          Future.successful(BadRequest(Json.obj("message" -> JsError.toFlatJson(errors))))
        },
        renewal => {
          errorWrapper(validateTcrAuthHeader() {
            token =>
                implicit hc =>
              service.submitRenewal(nino,renewal).map {
                case Error(status) => Status(status)(Json.toJson(ErrorwithNtcRenewal))
                case _ => Ok
              }

          })
        }
      )
  }

  final def taxCreditsSummary(nino: Nino) = accessControl.validateAccept(acceptHeaderValidationRules).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(service.getTaxCreditSummary(nino).map(as => Ok(Json.toJson(as))))
  }

  private def validateTcrAuthHeader()(func: String => HeaderCarrier => Future[mvc.Result])(implicit request:Request[_], hc:HeaderCarrier) = {
    request.headers.get(HeaderKeys.tcrAuthToken) match {
      case Some(token) => func(token)(hc.copy(extraHeaders = Seq(HeaderKeys.tcrAuthToken -> token)))

      case _ =>
        Logger.warn("Failed to find auth header")
        Future.successful(Forbidden(Json.toJson(ErrorNoAuthToken)))
    }
  }

}

object SandboxPersonalIncomeController extends PersonalIncomeController {
  override val service = SandboxPersonalIncomeService
  override val accessControl = AccountAccessControlCheckAccessOff
}

object LivePersonalIncomeController extends PersonalIncomeController {
  override val service = LivePersonalIncomeService
  override val accessControl = AccountAccessControlWithHeaderCheck
}
