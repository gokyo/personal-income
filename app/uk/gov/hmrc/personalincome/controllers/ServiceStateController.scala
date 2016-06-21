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


import uk.gov.hmrc.api.controllers.HeaderValidator
import uk.gov.hmrc.personalincome.controllers.action.{AccountAccessControlCheckOff, AccountAccessControlWithHeaderCheck}
import uk.gov.hmrc.personalincome.domain.{TaxCreditsSubmissions, TaxCreditsControl, TaxCreditsSubmissionControl}
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.time.DateTimeUtils

import scala.concurrent.Future

trait ServiceStateController extends BaseController with HeaderValidator with ErrorHandling {

  import play.api.libs.json.Json
  import uk.gov.hmrc.personalincome.domain.TaxCreditsSubmissions.formats
  import uk.gov.hmrc.play.http.HeaderCarrier

  import scala.concurrent.ExecutionContext.Implicits.global

  val taxCreditsSubmissionControlConfig : TaxCreditsControl
  val accessControl:AccountAccessControlWithHeaderCheck

  final def taxCreditsSubmissionState(journeyId: Option[String]=None) = accessControl.validateAccept(acceptHeaderValidationRules).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(
        Future {
          taxCreditsSubmissionControlConfig.toTaxCreditsSubmissions
        }.map{
          as => Ok(Json.toJson(as))
        })
  }
}


object SandboxServiceStateController extends ServiceStateController with DateTimeUtils {

  override val taxCreditsSubmissionControlConfig = new TaxCreditsControl {
    override def toTaxCreditsSubmissions = new TaxCreditsSubmissions(false, true)
  }
  override val accessControl = AccountAccessControlCheckOff
}

object LiveServiceStateController extends ServiceStateController {
  override val taxCreditsSubmissionControlConfig = TaxCreditsSubmissionControl
  override val accessControl = AccountAccessControlWithHeaderCheck
}
