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
import uk.gov.hmrc.personalincome.controllers.action.{AccountAccessControlCheckAccessOff, AccountAccessControlWithHeaderCheck}
import uk.gov.hmrc.personalincome.domain.{TaxCreditsSubmissionControl, TaxCreditsSubmissionControlConfig}
import uk.gov.hmrc.play.microservice.controller.BaseController
import uk.gov.hmrc.time.DateTimeUtils

import scala.concurrent.Future

trait ServiceStateController extends BaseController with HeaderValidator with ErrorHandling {

  import play.api.libs.json.Json
  import uk.gov.hmrc.personalincome.domain.TaxCreditsSubmissions.formats
  import uk.gov.hmrc.play.http.HeaderCarrier

  import scala.concurrent.ExecutionContext.Implicits.global

  val taxCreditsSubmissionControlConfig : TaxCreditsSubmissionControlConfig
  val accessControl:AccountAccessControlWithHeaderCheck

  final def taxCreditsRenewals() = accessControl.validateAccept(acceptHeaderValidationRules).async {
    implicit request =>
      implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers, None)
      errorWrapper(
        Future{
          taxCreditsSubmissionControlConfig.toTaxCreditsSubmissions
        }.map{
          as => Ok(Json.toJson(as))
        })
  }
}


object SandboxServiceStateController extends ServiceStateController with DateTimeUtils {
  import com.github.nscala_time.time.Imports._
  import com.typesafe.config.Config
  import uk.gov.hmrc.personalincome.domain.TaxCreditsSubmissionControl

  override val taxCreditsSubmissionControlConfig = new TaxCreditsSubmissionControlConfig{
    lazy val config: Config = ???
    override val submissionControl = new TaxCreditsSubmissionControl(false, now - 1.day, now + 1.day)
  }
  override val accessControl = AccountAccessControlCheckAccessOff
}

object LiveServiceStateController extends ServiceStateController {
  override val taxCreditsSubmissionControlConfig = TaxCreditsSubmissionControl
  override val accessControl = AccountAccessControlWithHeaderCheck
}
