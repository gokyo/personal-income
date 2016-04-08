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

package uk.gov.hmrc.apigateway.personalincome.controllers

import uk.gov.hmrc.apigateway.personalincome.controllers.action.{AccountAccessControlWithHeaderCheck, AccountAccessControlForSandbox}
import play.api.libs.json.Json
import uk.gov.hmrc.apigateway.personalincome.services.{LivePersonalIncomeService, PersonalIncomeService, SandboxPersonalIncomeService}
import uk.gov.hmrc.domain.Nino
import play.api.{mvc, Logger}
import uk.gov.hmrc.play.http.{HeaderCarrier, ForbiddenException, UnauthorizedException, NotFoundException}
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ErrorHandling {
  self:BaseController =>

  def errorWrapper(func: => Future[mvc.Result])(implicit hc:HeaderCarrier) = {
    func.recover {
      case ex:NotFoundException => Status(ErrorNotFound.httpStatusCode)(Json.toJson(ErrorNotFound))

      case ex:UnauthorizedException => Unauthorized(Json.toJson(ErrorUnauthorizedNoNino))

      case ex:ForbiddenException => Unauthorized(Json.toJson(ErrorUnauthorizedLowCL))

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
      errorWrapper(service.getSummary(nino,year).map(as => Ok(Json.toJson(as))))
  }
}

object SandboxPersonalIncomeController extends PersonalIncomeController {
  override val service = SandboxPersonalIncomeService
  override val accessControl = AccountAccessControlForSandbox
}

object LivePersonalIncomeController extends PersonalIncomeController {
  override val service = LivePersonalIncomeService
  override val accessControl = AccountAccessControlWithHeaderCheck
}
