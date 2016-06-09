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

import uk.gov.hmrc.personalincome.config.{ServicesCircuitBreaker, WSHttp}
import uk.gov.hmrc.personalincome.domain.TaxCreditsNino
import uk.gov.hmrc.personalincome.domain.userdata.{Children, PartnerDetails, PaymentSummary, PersonalDetails}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http.HttpGet

import scala.concurrent.{ExecutionContext, Future}

trait TaxCreditsBrokerConnector {
  this: ServicesCircuitBreaker =>

  val externalServiceName = "tax-credits-broker"

  def http: HttpGet

  def serviceUrl: String

  def url(nino:TaxCreditsNino, route:String) = s"$serviceUrl/tcs/${nino.value}/$route"

  def getPaymentSummary(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PaymentSummary] = {
    withCircuitBreaker((http.GET[PaymentSummary](url(nino, "payment-summary"))))
  }

  def getPersonalDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PersonalDetails] = {
    withCircuitBreaker(http.GET[PersonalDetails](url(nino, "personal-details")))
  }

  def getPartnerDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Option[PartnerDetails]] = {
    withCircuitBreaker(http.GET[Option[PartnerDetails]](url(nino, "partner-details")))
  }

  def getChildren(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Children] = {
    withCircuitBreaker(http.GET[Children](url(nino, "children")))
  }

}

object TaxCreditsBrokerConnector extends TaxCreditsBrokerConnector with ServicesConfig with ServicesCircuitBreaker {
  override def http:  WSHttp.type = WSHttp

  lazy val serviceUrl = baseUrl("tax-credits-broker")
}
