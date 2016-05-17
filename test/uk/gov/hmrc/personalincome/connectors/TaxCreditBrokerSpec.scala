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

import org.scalatest.concurrent.ScalaFutures
import uk.gov.hmrc.personalincome.domain.userdata._
import uk.gov.hmrc.personalincome.domain.TaxCreditsNino
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.http.{HeaderCarrier, _}
import uk.gov.hmrc.play.test.UnitSpec
import org.joda.time.DateTime

import scala.concurrent.{ExecutionContext, Future}

class TaxCreditBrokerSpec extends UnitSpec with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  private trait Setup {
    implicit lazy val hc = HeaderCarrier()

    val weekly = "WEEKLY"
    val expectedNextDueDate = DateTime.parse("2015-07-16")
    val expectedPaymentCTC = Payment(140.12, expectedNextDueDate, Some(weekly))
    val expectedPaymentWTC = Payment(160.34, expectedNextDueDate, Some(weekly))
    val paymentSummary = PaymentSummary(Some(expectedPaymentWTC), Some(expectedPaymentCTC))

    val AGE17="1999-08-31"
    val AGE18="1998-01-09"
    val AGE19="1997-01-09"

    val SarahSmith = Child("Sarah", "Smith", new DateTime(AGE17), false, false, true)
    val JosephSmith = Child("Joseph", "Smith", new DateTime(AGE18), false, false, true)
    val MarySmith = Child("Mary", "Smith", new DateTime(AGE19), false, false, true)

    val nino = Nino( "KM569110B")
    val address = Address("addressLine1", "addressLine2", Some("addressLine3"), Some("addressLine4"), "postcode")
    val personalDetails = PersonalDetails("Nuala",
                                          "O'Shea",
                                          TaxCreditsNino(nino.value),
                                          address,
                                          None, None, None, None)
    val partnerDetails = PartnerDetails("Frederick",
                                        Some("Tarquin"),
                                        "Hunter-Smith",
                                        TaxCreditsNino(nino.value),
                                        address,
                                        None,
                                        None,
                                        None,
                                        None)

    val children = Children(Seq(SarahSmith, JosephSmith, MarySmith))

    val connector = new TaxCreditsBrokerConnector {
      override def http: HttpGet = ???

      override def getPersonalDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PersonalDetails] = Future.successful(personalDetails)

      override def getPartnerDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Option[PartnerDetails]] = Future.successful(Some(partnerDetails))

      override def getChildren(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Children] = Future.successful(children)

      override def serviceUrl: String = ???

      override def getPaymentSummary(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[PaymentSummary]  = paymentSummary
    }
  }

  "taxCreditBroker connector" should {

    "return a valid response for getPersonalDetails when a 200 response is received with a valid json payload" in new Setup {
      await(connector.getPersonalDetails(TaxCreditsNino(nino.value))) shouldBe personalDetails
    }

    "return a valid response for getPartnerDetails when a 200 response is received with a valid json payload" in new Setup {
      await(connector.getPartnerDetails(TaxCreditsNino(nino.value))) shouldBe Some(partnerDetails)
    }

    "return a valid response for getChildren when a 200 response is received with a valid json payload" in new Setup {
      await(connector.getChildren(TaxCreditsNino(nino.value))) shouldBe children
    }

    "return a valid response for getPaymentSummary when a 200 response is received with a valid json payload" in new Setup {
      await(connector.getPaymentSummary(TaxCreditsNino(nino.value))) shouldBe paymentSummary
    }

  }

}
