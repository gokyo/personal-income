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

package uk.gov.hmrc.personalincome.stubs

import org.joda.time.DateTime
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.mockito.stubbing.OngoingStubbing
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.personalincome.connectors.TaxCreditsBrokerConnector
import uk.gov.hmrc.personalincome.domain.TaxCreditsNino
import uk.gov.hmrc.personalincome.domain.userdata._
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.time.DateTimeUtils

import scala.concurrent.{ExecutionContext, Future}

trait TaxCreditBrokerConnectorStub extends UnitSpec {

  val expectedNextDueDate = DateTime.parse("2015-07-16")
  val expectedPaymentWTC = FuturePayment(160.34, expectedNextDueDate, false)
  val expectedPaymentCTC = FuturePayment(140.12, expectedNextDueDate, false)
  val paymentSectionCTC = PaymentSection(List(expectedPaymentCTC), "weekly")
  val paymentSectionWTC = PaymentSection(List(expectedPaymentWTC), "weekly")
  val paymentSummary = PaymentSummary(Some(paymentSectionWTC), Some(paymentSectionCTC), true)

  val AGE16 = DateTimeUtils.now.minusYears(16)
  val AGE15 = DateTimeUtils.now.minusYears(15)
  val AGE13 = DateTimeUtils.now.minusYears(13)
  val AGE21 = DateTimeUtils.now.minusYears(21)
  val DECEASED_DATE = DateTimeUtils.now.minusYears(1)

  val SarahSmith = Child("Sarah", "Smith", new DateTime(AGE16), false, false, true, None)
  val JosephSmith = Child("Joseph", "Smith", new DateTime(AGE15), false, false, true, None)
  val MarySmith = Child("Mary", "Smith", new DateTime(AGE13), false, false, true, None)
  val JennySmith = Child("Jenny", "Smith", new DateTime(AGE21), false, false, true, None)
  val PeterSmith = Child("Peter", "Smith", new DateTime(AGE13), false, false, false, Some(new DateTime(DECEASED_DATE)))
  val SimonSmith = Child("Simon", "Smith", new DateTime(AGE13), false, false, true, Some(new DateTime(DECEASED_DATE)))

  val address = uk.gov.hmrc.personalincome.domain.userdata.Address("addressLine1", "addressLine2", Some("addressLine3"), Some("addressLine4"), Some("postcode"))

  def personalDetails(nino: String) = PersonalDetails("firstname", "surname", TaxCreditsNino(nino), address, None, None, None, None)

  def partnerDetails(nino: String) = PartnerDetails("forename", Some("othernames"), "surname", TaxCreditsNino(nino), address, None, None, None, None)

  def stubTaxCreditBrokerConnectorGetChildren(response: Children)(implicit taxCreditBrokerConnector: TaxCreditsBrokerConnector) : OngoingStubbing[Future[Children]]= {
    when(taxCreditBrokerConnector.getChildren(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(response)
  }

  def stubTaxCreditBrokerConnectorGetExclusion(response: Exclusion)(implicit taxCreditBrokerConnector: TaxCreditsBrokerConnector) : OngoingStubbing[Future[Exclusion]]= {
    when(taxCreditBrokerConnector.getExclusion(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(response)
  }

  def stubTaxCreditBrokerConnectorGetPartnerDetails(response: Option[PartnerDetails])(implicit taxCreditBrokerConnector: TaxCreditsBrokerConnector) : OngoingStubbing[Future[Option[PartnerDetails]]]= {
    when(taxCreditBrokerConnector.getPartnerDetails(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(response)
  }

  def stubTaxCreditBrokerConnectorGetPersonalDetails(response: PersonalDetails)(implicit taxCreditBrokerConnector: TaxCreditsBrokerConnector) : OngoingStubbing[Future[PersonalDetails]]= {
    when(taxCreditBrokerConnector.getPersonalDetails(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(response)
  }

  def stubTaxCreditBrokerConnectorGetPaymentSummary(response: PaymentSummary)(implicit taxCreditBrokerConnector: TaxCreditsBrokerConnector) : OngoingStubbing[Future[PaymentSummary]]= {
    when(taxCreditBrokerConnector.getPaymentSummary(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(response)
  }

}
