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

package uk.gov.hmrc.personalincome.connectors

import play.api.Logger
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.config.{ServicesCircuitBreaker, WSHttp}
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}

trait PersonalTaxSummaryConnector {
  this: ServicesCircuitBreaker =>

  val externalServiceName = "personal-tax-summary"

  def http: HttpGet with HttpPost

  def serviceUrl: String

  def url(path: String) = s"$serviceUrl$path"

  def taxSummary(nino: Nino, year : Int)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TaxSummaryContainer]] = {
    Logger.debug(s"PersonalTaxSummary - connect to /$nino/summary/$year ")
    withCircuitBreaker(
      http.GET[Option[TaxSummaryContainer]](url = url(s"/personal-tax/$nino/summary/$year")).recover {
        case ex: NotFoundException => None

        case ex: BadRequestException => None
      }
    )
  }
}

object PersonalTaxSummaryConnector extends PersonalTaxSummaryConnector with ServicesConfig with ServicesCircuitBreaker {
  lazy val serviceUrl = baseUrl("personal-tax-summary")
  override def http = WSHttp
}
