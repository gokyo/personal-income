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

package unit.controllers

import connectors.TaiConnector
import controllers.PersonalIncomeController
import domain.TaxSummaryDetails
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeApplication, FakeRequest}
import services.LivePersonalIncomeService
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpPost}
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.Future


class TestPersonalIncomeSpec extends UnitSpec with WithFakeApplication with ScalaFutures with BeforeAndAfterEach with StubApplicationConfiguration {
  implicit val hc = HeaderCarrier()

  class TestPersonalIncomeController(taxSummaryDetails:TaxSummaryDetails) extends PersonalIncomeController {

    class TestConnector extends TaiConnector {
      override def http: HttpGet with HttpPost = ???

      override def serviceUrl: String = ???

      override def taxSummary(nino : Nino, year : Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
        Future.successful(taxSummaryDetails)
      }
    }

    class TestService extends LivePersonalIncomeService {
      var saveNino=""
      var saveYear=0

      override val connector = new TestConnector

      override def audit(method:String, nino:Nino, year:Int, details:Map[String, String]): Unit = {
        saveNino=nino.value
        saveYear=year
      }
    }

    override val service = new TestService
    override implicit val hc: HeaderCarrier = HeaderCarrier()
  }

  override lazy val fakeApplication = FakeApplication(additionalConfiguration=config)

  val emptyRequest = FakeRequest()
  val emptyRequestWithHeader = FakeRequest().withHeaders("Accept" -> "application/vnd.hmrc.1.0+json")

  val nino = Nino( "KM569110B")
  val taxSummaryDetails = TaxSummaryDetails(nino.value,1)

  "getSummary" should {
    "return the summary successfully" in {

      val controller = new TestPersonalIncomeController(taxSummaryDetails)
      val result = await(controller.getSummary(nino,90)(emptyRequestWithHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxSummaryDetails)

      controller.service.saveNino shouldBe nino.value
      controller.service.saveYear shouldBe 90
    }

    "return status code 406 when the headers are invalid" in {
      val controller = new TestPersonalIncomeController(taxSummaryDetails)
      val result = await(controller.getSummary(nino,90)(emptyRequest))

      status(result) shouldBe 406
    }

  }
}
