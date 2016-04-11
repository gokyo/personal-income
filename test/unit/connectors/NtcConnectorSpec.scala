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

import models.{IncomeDetails, RenewalData, TcrRenewal}
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.apigateway.personalincome.connectors.NtcConnector
import uk.gov.hmrc.apigateway.personalincome.domain.TaxCreditsNino
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.ws.WSPost
import uk.gov.hmrc.play.http.{HeaderCarrier, _}
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class NtcConnectorSpec
  extends UnitSpec
          with ScalaFutures {

  private trait Setup {

    implicit lazy val hc = HeaderCarrier()

    val nino = Nino( "KM569110B")
    val taxCreditNino = TaxCreditsNino(nino.value)
    val incomeDetails = IncomeDetails(Some(10), Some(20), Some(30), Some(40), Some(true))
    val renewal = TcrRenewal(RenewalData(Some(incomeDetails), None, None), None, None, None, false)

    lazy val http500Response = Future.failed(new Upstream5xxResponse("Error", 500, 500))
    lazy val http400Response = Future.failed(new BadRequestException("bad request"))
    lazy val http200Response = Future.successful(HttpResponse(200, Some(Json.toJson(renewal))))

    lazy val response: Future[HttpResponse] = http400Response

    val connector = new NtcConnector {
      override lazy val serviceUrl = "someUrl"
      override lazy val http: WSPost = new WSPost {
        override val hooks: Seq[HttpHook] = NoneRequired
        override def doPost[A](url: String, body: A, headers: Seq[(String, String)])(implicit wts: Writes[A], hc: HeaderCarrier): Future[HttpResponse] = response
      }
    }
  }

  "tcsConnector" should {

    "throw BadRequestException when a 400 response is returned" in new Setup {
      override lazy val response = http400Response
        intercept[BadRequestException] {
          await(connector.submitRenewal(taxCreditNino, renewal))
      }
    }

    "throw Upstream5xxResponse when a 500 response is returned" in new Setup {
      override lazy val response = http500Response
      intercept[Upstream5xxResponse] {
        await(connector.submitRenewal(taxCreditNino, renewal))
      }
    }

    "return a valid response when a 200 response is received with a valid json payload" in new Setup {
      override lazy val response = http200Response
      val result = await(connector.submitRenewal(taxCreditNino, renewal))
      result.status shouldBe 200
    }
  }

}
