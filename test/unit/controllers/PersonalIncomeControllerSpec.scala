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

import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.FakeApplication
import uk.gov.hmrc.apigateway.personalincome.domain.TaxSummaryDetails
import uk.gov.hmrc.apigateway.personalincome.services.SandboxPersonalIncomeService._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class TestPersonalIncomeSummarySpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "getSummary Live" should {

    "return the summary successfully" in new Success {

      val result = await(controller.getSummary(nino,90)(emptyRequestWithHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxSummaryDetails)

      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value, "year" -> "90")
    }

    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.getSummary(nino,90)(emptyRequestWithHeader))

      status(result) shouldBe 401
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.getSummary(nino,90)(emptyRequest))

      status(result) shouldBe 406
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }
  }

  "getSummary Sandbox" should {

    "return the summary response from a resource" in new SandboxSuccess {
      val year = 2016
      val result = await(controller.getSummary(nino, 2016)(emptyRequestWithHeader))

      status(result) shouldBe 200

      val resource = findResource(s"/resources/getsummary/${nino.value}_$year.json")
      contentAsJson(result) shouldBe Json.parse(resource.get)

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return the static resource since the supplied resource cannot be resolved" in new SandboxSuccess {
      val year = 2016
      val result = await(controller.getSummary(nino, 2018)(emptyRequestWithHeader))

      status(result) shouldBe 200

      contentAsJson(result) shouldBe Json.toJson(TaxSummaryDetails(nino.value, 2018))

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

  }
}

class TestPersonalIncomeRenewalSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "renewal Live" should {

    "process the renewal successfully" in new Success {
      val result = await(controller.submitRenewal(nino)(renewalRequest))

      status(result) shouldBe 200
    }

    "return bad request when invalid json is submitted" in new Success {
      val result = await(controller.submitRenewal(nino)(renewalBadRequest))

      status(result) shouldBe 400
    }

    "Return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.submitRenewal(nino)(renewalRequest))

      status(result) shouldBe 401
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.submitRenewal(nino)(renewalRequestNoAcceptHeader))

      status(result) shouldBe 406
    }
  }

  "renewal sandbox" should {

    "return success response" in new SandboxSuccess {
      val result = await(controller.submitRenewal(nino)(renewalRequest))

      status(result) shouldBe 200
    }

    "return status code 406 when the headers are invalid" in new SandboxSuccess {
      val result = await(controller.submitRenewal(nino)(renewalRequestNoAcceptHeader))

      status(result) shouldBe 406
    }
  }

}
