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

import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.test.FakeApplication
import uk.gov.hmrc.personalincome.config.ServicesCircuitBreaker
import uk.gov.hmrc.personalincome.controllers.StubApplicationConfiguration
import uk.gov.hmrc.personalincome.domain.userdata._
import uk.gov.hmrc.personalincome.domain.TaxCreditsNino
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.test.{WithFakeApplication, UnitSpec}
import org.joda.time.DateTime

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global


class TaxCreditsBrokerTestConnector(response:Option[Future[HttpResponse]]=None) extends TaxCreditsBrokerConnector with ServicesConfig with ServicesCircuitBreaker {

  override lazy val http: HttpGet = new HttpGet {
    override val hooks: Seq[HttpHook] = NoneRequired

    override protected def doGet(url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = response.getOrElse(throw new Exception("No response defined!"))
  }

  override def serviceUrl: String = "some-url"
}

class TaxCreditBrokerSpec extends UnitSpec with ScalaFutures with StubApplicationConfiguration with WithFakeApplication with CircuitBreakerTest {

override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  trait Setup {
    implicit lazy val hc = HeaderCarrier()

    val weekly = "WEEKLY"
    val expectedNextDueDate = DateTime.parse("2015-07-16")

    val expectedPaymentWTC = Payment(160.34, expectedNextDueDate, false)
    val expectedPaymentCTC = Payment(140.12, expectedNextDueDate, false)
    val paymentSectionCTC = PaymentSection(List(expectedPaymentCTC), "weekly")
    val paymentSectionWTC = PaymentSection(List(expectedPaymentWTC), "weekly")
    val paymentSummary = PaymentSummary(Some(paymentSectionWTC), Some(paymentSectionCTC), true)


    lazy val http500Response = Future.failed(new Upstream5xxResponse("Error", 500, 500))
    lazy val http400Response = Future.failed(new BadRequestException("bad request"))
    lazy val http204Response = Future.successful(HttpResponse(204))
    lazy val response: Future[HttpResponse] = http200Person

    lazy val http200Person = Future.successful(HttpResponse(200, Some(Json.toJson(personalDetails))))
    lazy val http200Partner = Future.successful(HttpResponse(200, Some(Json.toJson(partnerDetails))))
    lazy val http200Children = Future.successful(HttpResponse(200, Some(Json.toJson(children))))
    lazy val http200Payment = Future.successful(HttpResponse(200, Some(Json.toJson(paymentSummary))))
    lazy val http200Exclusion = Future.successful(HttpResponse(200, Some(Json.toJson(exclusion))))


    val AGE17="1999-08-31"
    val AGE18="1998-01-09"
    val AGE19="1997-01-09"

    val SarahSmith = Child("Sarah", "Smith", new DateTime(AGE17), false, false, false, None)
    val JosephSmith = Child("Joseph", "Smith", new DateTime(AGE18), false, false, false, None)
    val MarySmith = Child("Mary", "Smith", new DateTime(AGE19), false, false, false, None)

    val nino = Nino( "KM569110B")
    val address = Address("addressLine1", "addressLine2", Some("addressLine3"), Some("addressLine4"), Some("postcode"))
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

    val exclusion = Exclusion(true)

    val connector = new TaxCreditsBrokerTestConnector(Some(response))
  }

  trait BackwardsCompatibleSetup extends Setup {

    lazy val backwardsCompatibleResponse: Future[HttpResponse] = http200PaymentOld
    val expectedPaymentCTCOld = PaymentOld(140.12, expectedNextDueDate, Some("WEEKLY"))
    val expectedPaymentWTCOld = PaymentOld(160.34, expectedNextDueDate, Some("WEEKLY"))
    val paymentSummaryOld = PaymentSummaryOld(Some(expectedPaymentWTCOld), Some(expectedPaymentCTCOld))
    val http200PaymentOld = Future.successful(HttpResponse(200, Some(Json.toJson(paymentSummaryOld))))
    override val connector = new TaxCreditsBrokerTestConnector(Some(backwardsCompatibleResponse))
  }

  "taxCreditBroker connector" should {

    "return a valid response for getPersonalDetails when a 200 response is received with a valid json payload" in new Setup {
      override lazy val response = http200Person

      await(connector.getPersonalDetails(TaxCreditsNino(nino.value))) shouldBe personalDetails
    }

    "return a valid response for getPartnerDetails when a 200 response is received with a valid json payload" in new Setup {
      override lazy val response = http200Partner

      await(connector.getPartnerDetails(TaxCreditsNino(nino.value))) shouldBe Some(partnerDetails)
    }

    "return a valid response for getChildren when a 200 response is received with a valid json payload" in new Setup {
      override lazy  val response = http200Children

      await(connector.getChildren(TaxCreditsNino(nino.value))) shouldBe children
    }

    "return exclusion when 200 response is received with a valid json payload" in new Setup {
      override lazy  val response = http200Exclusion

      await(connector.getExclusion(TaxCreditsNino(nino.value))) shouldBe exclusion
    }


    "return a valid response for getPaymentSummary when a 200 response is received with a valid json payload" in new Setup {
      override lazy val response = http200Payment

      val result = await(connector.getPaymentSummary(TaxCreditsNino(nino.value)))
      result.isRight shouldBe true
      result.right.get shouldBe paymentSummary
    }

    "return a valid response for getPaymentSummary when a 200 response is received with a valid json payload - Backwards Compatible" in new BackwardsCompatibleSetup {

      val result = await(connector.getPaymentSummary(TaxCreditsNino(nino.value)))
      result.isLeft shouldBe true
      result.left.get shouldBe paymentSummaryOld
    }

    "circuit breaker configuration should be applied and unhealthy service exception will kick in after 5th failed call" in new Setup {
      override lazy val response = http500Response
      executeCB(connector.getPaymentSummary(TaxCreditsNino(nino.value)))
    }


  }

}
