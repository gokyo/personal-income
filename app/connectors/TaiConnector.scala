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

package connectors

import config.WSHttp
import domain.TaxSummaryDetails
import play.api.Logger
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.Future


trait TaiConnector {
  def http: HttpGet with HttpPost

  def serviceUrl: String

  def url(path: String) = s"$serviceUrl$path"

  def taxSummary(nino : Nino, year : Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    Logger.debug(s"TaxForCitizens:Frontend - connect to /$nino/tax-summary-full/$year ")
    http.GET[TaxSummaryDetails](url = url(s"/tai/$nino/tax-summary-full/$year"))
  }
}

object TaiConnector extends TaiConnector with ServicesConfig {
  lazy val serviceUrl = baseUrl("tai")
  override def http = WSHttp
}
