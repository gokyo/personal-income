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

package uk.gov.hmrc.personalincome.domain

import org.joda.time.{DateTime, DateTimeZone}
import play.api.libs.json.Json
import uk.gov.hmrc.time.DateTimeUtils

case class TaxCreditsSubmissions(shuttered : Boolean, inSubmissionPeriod : Boolean)

case class SubmissionState(submissionState: Boolean)

object TaxCreditsSubmissions extends DateTimeUtils {
  implicit val formats = Json.format[TaxCreditsSubmissions]
}

object SubmissionState {
  implicit val formats = Json.format[SubmissionState]
}

trait LoadConfig {

  import com.typesafe.config.Config

  def config: Config
}

trait TaxCreditsControl {
  def toTaxCreditsSubmissions: TaxCreditsSubmissions
  def toSubmissionState: SubmissionState
}

trait TaxCreditsSubmissionControlConfig extends TaxCreditsControl with LoadConfig with DateTimeUtils {
  import net.ceedubs.ficus.readers.ValueReader
  import net.ceedubs.ficus.Ficus._

  private val submission = "microservice.services.ntc.submission"

  private implicit val nativeVersionReader: ValueReader[TaxCreditsSubmissionControl] = ValueReader.relative { nativeVersion =>
    TaxCreditsSubmissionControl(
      config.as[Boolean](s"$submission.shutter"),
      DateTime.parse(config.as[String](s"$submission.startDate")).toDateTime(DateTimeZone.UTC).withTimeAtStartOfDay(),
      DateTime.parse(config.as[String](s"$submission.endDate")).toDateTime(DateTimeZone.UTC)
    )
  }

  val submissionControl: TaxCreditsSubmissionControl = config.as[TaxCreditsSubmissionControl](submission)

  def toTaxCreditsSubmissions : TaxCreditsSubmissions = {
    val currentTime = now.getMillis
    val allowSubmissions = currentTime >= submissionControl.startMs && currentTime <= submissionControl.endMs
    new TaxCreditsSubmissions(submissionControl.shutter, allowSubmissions)
  }

  def toSubmissionState : SubmissionState = {
    new SubmissionState(!toTaxCreditsSubmissions.shuttered && toTaxCreditsSubmissions.inSubmissionPeriod)
  }
}


sealed case class TaxCreditsSubmissionControl(shutter : Boolean, startDate : DateTime, endDate : DateTime){
  val startMs : Long = startDate.getMillis
  val endMs : Long = endDate.getMillis
}

object TaxCreditsSubmissionControl extends TaxCreditsSubmissionControlConfig {
  import com.typesafe.config.{Config, ConfigFactory}

  lazy val config: Config = ConfigFactory.load()

}
