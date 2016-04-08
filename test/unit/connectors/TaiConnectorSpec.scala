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

package unit.connectors

import uk.gov.hmrc.apigateway.personalincome.connectors.TaiConnector
import uk.gov.hmrc.apigateway.personalincome.domain.TaxSummaryDetails
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.{HeaderCarrier, _}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class TaiConnectorSpec
  extends UnitSpec
          with ScalaFutures {

  private trait Setup {

    implicit lazy val hc = HeaderCarrier()

    val nino = Nino( "KM569110B")
    val taxSummary = TaxSummaryDetails(nino.value,1)

    lazy val http500Response = Future.failed(new Upstream5xxResponse("Error", 500, 500))
    lazy val http400Response = Future.failed(new BadRequestException("bad request"))
    lazy val http200Response = Future.successful(HttpResponse(200, Some(Json.toJson(taxSummary))))

    lazy val response: Future[HttpResponse] = http400Response

    val connector = new TaiConnector {
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

  "taiConnector" should {

    "throw BadRequestException when a 400 response is returned" in new Setup {
      override lazy val response = http400Response
        intercept[BadRequestException] {
          await(connector.taxSummary(nino, 1))
      }
    }

    "throw Upstream5xxResponse when a 500 response is returned" in new Setup {
      override lazy val response = http500Response
      intercept[Upstream5xxResponse] {
        await(connector.taxSummary(nino, 1))
      }
    }

    "return a valid resoonse when a 200 response is received with a valid json payload" in new Setup {
      override lazy val response = http200Response
      await(connector.taxSummary(nino, 1)) shouldBe taxSummary
    }
  }

}
