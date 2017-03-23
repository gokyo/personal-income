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
import play.api.http.Status._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.config.{ServicesCircuitBreaker, WSHttp}
import uk.gov.hmrc.personalincome.domain.TaxSummaryDetails
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}


trait TaiConnector {
  this: ServicesCircuitBreaker =>

  val externalServiceName = "tai"

  def http: HttpGet with HttpPost

  def serviceUrl: String

  def url(path: String) = s"$serviceUrl$path"

  def taxSummary(nino: Nino, year: Int)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TaxSummaryDetails]] = {
    withCircuitBreaker(
      http.GET[HttpResponse](url = url(s"/tai/$nino/tax-summary-full/$year")) map { response =>
        response.status match {
          case OK => Some(response.json.as[TaxSummaryDetails](TaxSummaryDetails.formats))
          case nonOkResponse => {
            Logger.warn(s"taxSummary request responded with $nonOkResponse")
            None
          }
        }
      } recover {
        case ex: NotFoundException => None
        case ex: BadRequestException => None
      }
    )
  }
}

object TaiConnector extends TaiConnector with ServicesConfig with ServicesCircuitBreaker {
  lazy val serviceUrl = baseUrl("tai")

  override def http = WSHttp
}
