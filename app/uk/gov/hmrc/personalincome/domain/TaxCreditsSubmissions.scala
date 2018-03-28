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

package uk.gov.hmrc.personalincome.domain

import org.joda.time.DateTime
import org.joda.time.DateTime.parse
import org.joda.time.DateTimeZone.UTC
import play.api.libs.json.Json
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.time.DateTimeUtils

case class TaxCreditsSubmissions(submissionShuttered : Boolean, inSubmitRenewalsPeriod : Boolean, inViewRenewalsPeriod : Boolean){
  def toTaxCreditsRenewalsState = {
    new TaxCreditsRenewalsState(
      if (inSubmitRenewalsPeriod) {
        if (submissionShuttered) {
          "shuttered"
        } else {
          "open"
        }
      } else if (inViewRenewalsPeriod) {
        "check_status_only"
      } else {
        "closed"
      }
    )
  }
}

case class TaxCreditsRenewalsState(submissionsState: String)

object TaxCreditsSubmissions extends DateTimeUtils {
  implicit val formats = Json.format[TaxCreditsSubmissions]
}

object TaxCreditsRenewalsState {
  implicit val formats = Json.format[TaxCreditsRenewalsState]
}

trait LoadConfig {

  import com.typesafe.config.Config

  def config: Config
}

trait TaxCreditsControl {
  def toTaxCreditsSubmissions: TaxCreditsSubmissions
  def toTaxCreditsRenewalsState: TaxCreditsRenewalsState
}

trait TaxCreditsSubmissionControlConfig extends TaxCreditsControl with ServicesConfig with DateTimeUtils {

  private val submission = "microservice.services.ntc.submission"

  val submissionControl: TaxCreditsSubmissionControl =
    TaxCreditsSubmissionControl(
      getBoolean(s"$submission.submissionShuttered"),
      parse(getString(s"$submission.startDate")).toDateTime(UTC),
      parse(getString(s"$submission.endDate")).toDateTime(UTC),
      parse(getString(s"$submission.endViewRenewalsDate")).toDateTime(UTC)
    )

  def toTaxCreditsSubmissions : TaxCreditsSubmissions = {
    val currentTime = now.getMillis
    val allowSubmissions = currentTime >= submissionControl.startMs && currentTime <= submissionControl.endMs
    val allowViewSubmissions = currentTime >= submissionControl.startMs && currentTime <= submissionControl.endViewMs
    new TaxCreditsSubmissions(submissionControl.submissionShuttered, allowSubmissions, allowViewSubmissions)
  }

  def toTaxCreditsRenewalsState : TaxCreditsRenewalsState = {
    toTaxCreditsSubmissions.toTaxCreditsRenewalsState
  }
}


sealed case class TaxCreditsSubmissionControl(
  submissionShuttered : Boolean, startDate : DateTime, endDate : DateTime, endViewRenewalsDate : DateTime){
  val startMs : Long = startDate.getMillis
  val endMs : Long = endDate.getMillis
  val endViewMs : Long = endViewRenewalsDate.getMillis
}

object TaxCreditsSubmissionControl extends TaxCreditsSubmissionControlConfig {
  import com.typesafe.config.{Config, ConfigFactory}

  lazy val config: Config = ConfigFactory.load()

}
