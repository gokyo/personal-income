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
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.config.ServicesCircuitBreaker
import uk.gov.hmrc.personalincome.controllers.StubApplicationConfiguration
import uk.gov.hmrc.personalincome.domain.TaxSummaryDetails
import uk.gov.hmrc.personaltaxsummary.domain.PersonalTaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.viewmodels._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class PersonalTaxSummaryTestConnector extends PersonalTaxSummaryConnector with ServicesConfig with ServicesCircuitBreaker {
  override def http: HttpGet with HttpPost = ???

  override def serviceUrl: String = "some-service-url"
}

class PersonalTaxSummaryConnectorSpec
  extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration with CircuitBreakerTest {

  private trait Setup {

    implicit lazy val hc = HeaderCarrier()

    val nino = Nino("KM569110B")
    val taxSummary = TaxSummaryDetails(nino.value, 1)
    val taxSummaryContainer = PersonalTaxSummaryContainer(taxSummary, Map.empty)
    val incomeTax = IncomeTaxViewModel(simpleTaxUser = true)
    val estimatedIncome = PTSEstimatedIncomeViewModel(graph = BandedGraph("1"), taxBands = None, ukDividends = None, incomeTaxReducedToZeroMessage = None, newGraph = BandedGraph("1"))
    val yourTaxableIncome = PTSYourTaxableIncomeViewModel(0, 0, 0, List(), None, EmploymentPension(None), List(), 0, List(), 0, List(), 0, List(), 0, false, newGraph = BandedGraph("1"))

    lazy val http500Response = Future.failed(new Upstream5xxResponse("Error", 500, 500))
    lazy val http400Response = Future.failed(new BadRequestException("bad request"))
    lazy val http404Response = Future.failed(new NotFoundException("not found"))
    lazy val http200EstimatedIncomeResponse = Future.successful(HttpResponse(200, Some(Json.toJson(estimatedIncome))))
    lazy val http200YourTaxableIncomeResponse = Future.successful(HttpResponse(200, Some(Json.toJson(yourTaxableIncome))))
    lazy val response: Future[HttpResponse] = http400Response

    val connector = new PersonalTaxSummaryTestConnector {
      override lazy val serviceUrl = "someUrl"
      override lazy val http: HttpGet with HttpPost = new HttpGet with HttpPost {
        override val hooks: Seq[HttpHook] = NoneRequired
        override protected def doGet(url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = ???

        override protected def doPost[A](url: String, body: A, headers: Seq[(String, String)])(implicit wts: Writes[A], hc: HeaderCarrier): Future[HttpResponse] = response

        override protected def doPostString(url: String, body: String, headers: Seq[(String, String)])(implicit hc: HeaderCarrier): Future[HttpResponse] = ???

        override protected def doFormPost(url: String, body: Map[String, Seq[String]])(implicit hc: HeaderCarrier): Future[HttpResponse] = ???

        override protected def doEmptyPost[A](url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = ???
      }
    }
  }

  "PersonalTaxSummaryConnector.buildEstimatedIncome" should {

    "throw Upstream5xxResponse when a 500 response is returned" in new Setup {
      override lazy val response = http500Response
      intercept[Upstream5xxResponse] {
        await(connector.buildEstimatedIncome(nino, taxSummaryContainer))
      }
    }

    "return a valid response when a 200 response is received with a valid json payload" in new Setup {
      override lazy val response = http200EstimatedIncomeResponse
      await(connector.buildEstimatedIncome(nino, taxSummaryContainer)) shouldBe estimatedIncome
    }

    "circuit breaker configuration should be applied and unhealthy service exception will kick in after 5th failed call" in new Setup {
      override lazy val response = http500Response
      executeCB(connector.buildEstimatedIncome(nino, taxSummaryContainer))
    }

  }

  "PersonalTaxSummaryConnector.buildYourTaxableIncome" should {

    "throw Upstream5xxResponse when a 500 response is returned" in new Setup {
      override lazy val response = http500Response
      intercept[Upstream5xxResponse] {
        await(connector.buildYourTaxableIncome(nino, taxSummaryContainer))
      }
    }

    "return a valid response when a 200 response is received with a valid json payload" in new Setup {
      override lazy val response = http200YourTaxableIncomeResponse
      await(connector.buildYourTaxableIncome(nino, taxSummaryContainer)) shouldBe yourTaxableIncome
    }

    "circuit breaker configuration should be applied and unhealthy service exception will kick in after 5th failed call" in new Setup {
      override lazy val response = http500Response
      executeCB(connector.buildYourTaxableIncome(nino, taxSummaryContainer))
    }

  }
}
