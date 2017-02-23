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
import uk.gov.hmrc.model.TaxSummaryDetails
import uk.gov.hmrc.personalincome.config.ServicesCircuitBreaker
import uk.gov.hmrc.personalincome.controllers.StubApplicationConfiguration
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.viewmodels.IncomeTaxViewModel
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

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
    val taxSummary = TaxSummaryDetails(nino.value,1)
    val incomeTax = IncomeTaxViewModel(simpleTaxUser = true)
    val taxSummaryContainer = TaxSummaryContainer(taxSummary, incomeTax, None, None, None)

    lazy val http500Response = Future.failed(new Upstream5xxResponse("Error", 500, 500))
    lazy val http400Response = Future.failed(new BadRequestException("bad request"))
    lazy val http404Response = Future.failed(new NotFoundException("not found"))
    lazy val http200Response = Future.successful(HttpResponse(200, Some(Json.toJson(taxSummaryContainer))))
    lazy val response: Future[HttpResponse] = http400Response

    val connector = new PersonalTaxSummaryTestConnector {
      override lazy val serviceUrl = "someUrl"
      override lazy val http: HttpGet with HttpPost = new HttpGet with HttpPost {
        override val hooks: Seq[HttpHook] = NoneRequired
        override protected def doGet(url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = response

        override protected def doPost[A](url: String, body: A, headers: Seq[(String, String)])(implicit wts: Writes[A], hc: HeaderCarrier): Future[HttpResponse] = ???

        override protected def doPostString(url: String, body: String, headers: Seq[(String, String)])(implicit hc: HeaderCarrier): Future[HttpResponse] = ???

        override protected def doFormPost(url: String, body: Map[String, Seq[String]])(implicit hc: HeaderCarrier): Future[HttpResponse] = ???

        override protected def doEmptyPost[A](url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = ???
      }
    }
  }


  //TODO add tests for buildEstimatedIncome and buildYourTaxableIncome.


}
