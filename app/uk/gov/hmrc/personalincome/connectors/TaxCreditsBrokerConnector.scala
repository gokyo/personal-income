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

package uk.gov.hmrc.personalincome.connectors

import uk.gov.hmrc.personalincome.config.WSHttp
import uk.gov.hmrc.personalincome.domain.TaxCreditsNino
import uk.gov.hmrc.personalincome.domain.userdata.{Children, PartnerDetails, PaymentSummary, PersonalDetails}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.HttpGet

import scala.concurrent.{ExecutionContext, Future}

trait TaxCreditsBrokerConnector {
  def http: HttpGet

  def serviceUrl: String

  def getPaymentSummary(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PaymentSummary]
  def getPersonalDetails(nino:TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PersonalDetails]
  def getPartnerDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Option[PartnerDetails]]
  def getChildren(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Children]
}

object TaxCreditsBrokerConnector extends TaxCreditsBrokerConnector with ServicesConfig {
  override def http:  WSHttp.type = WSHttp

  lazy val serviceUrl = baseUrl("tax-credits-broker")

  def url(nino:TaxCreditsNino, route:String) = s"$serviceUrl/tcs/${nino.value}/$route"

  override def getPaymentSummary(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PaymentSummary] = {
    http.GET[PaymentSummary](url(nino, "payment-summary"))
  }

  override def getPersonalDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PersonalDetails] = {
    http.GET[PersonalDetails](url(nino, "personal-details"))
  }

  override def getPartnerDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Option[PartnerDetails]] = {
    http.GET[Option[PartnerDetails]](url(nino, "partner-details"))
  }

  override def getChildren(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Children] = {
    http.GET[Children](url(nino, "children"))
  }

}
