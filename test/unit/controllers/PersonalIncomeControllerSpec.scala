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
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.FakeApplication
import uk.gov.hmrc.apigateway.personalincome.controllers.ErrorNoAuthToken
import uk.gov.hmrc.apigateway.personalincome.domain.{BaseViewModel, TaxSummaryContainer, TaxSummaryDetails}
import uk.gov.hmrc.apigateway.personalincome.services.SandboxPersonalIncomeService._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}


// TODO...add common functions to test result + json messages
class TestPersonalIncomeSummarySpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "getSummary Live" should {

    "return the summary successfully" in new Success {

      val result: Result = await(controller.getSummary(nino,90)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxSummaryContainer)

      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value, "year" -> "90")
    }

    "return the gateKeeper summary successfully" in new GateKeeper {

      val result: Result = await(controller.getSummary(nino,90)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxSummaryContainerGK)

      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value, "year" -> "90")
    }


    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.getSummary(nino,90)(emptyRequestWithAcceptHeader))

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
      val result = await(controller.getSummary(nino, year)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200

      val resource = findResource(s"/resources/getsummary/${nino.value}_$year.json")
      contentAsJson(result) shouldBe Json.parse(resource.get)

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return the static resource since the supplied resource cannot be resolved" in new SandboxSuccess {
      val year = 2018
      val result = await(controller.getSummary(nino, year)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200

      contentAsJson(result) shouldBe Json.toJson(TaxSummaryContainer(TaxSummaryDetails(nino.value, year), BaseViewModel(estimatedIncomeTax=0), None, None, None))

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }
  }
}

class TestPersonalIncomeRenewalAuthenticateSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "authenticate Live" should {

    "process the authentication successfully" in new Success {
      val  result = await(controller.getRenewalAuthentication(nino, renewalReference)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(tcrAuthToken)
    }

    "Return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val  result = await(controller.getRenewalAuthentication(nino, renewalReference)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
    }

    "return status code 406 when the headers are invalid" in new Success {
      val  result = await(controller.getRenewalAuthentication(nino, renewalReference)(emptyRequest))

      status(result) shouldBe 406
    }

  }
}


class TestPersonalIncomeRenewalClaimantDetailsSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "claimentDetails Live" should {

    "return claimentDetails successfully" in new Success {
      val result = await(controller.claimentDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimentDetails)
    }

    "Return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.claimentDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader))

      status(result) shouldBe 401
    }

    "return 403 response when the tcr auth header is not supplied in the request" in new Success {
      val result = await(controller.claimentDetails(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.toJson(ErrorNoAuthToken)
    }

    "return status code 406 when the Accept header is invalid" in new Success {
      val  result = await(controller.claimentDetails(nino)(emptyRequest))

      status(result) shouldBe 406
    }

  }

  "claimentDetails Sandbox" should {

    "return claimentDetails successfully" in new Success {
      val result = await(controller.claimentDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimentDetails)
    }

    "return 403 response when the tcr auth header is not supplied in the request" in new Success {
      val result = await(controller.claimentDetails(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 403

      contentAsJson(result) shouldBe Json.toJson(ErrorNoAuthToken)
    }

    "return status code 406 when the Accept header is invalid" in new Success {
      val  result = await(controller.claimentDetails(nino)(emptyRequest))

      status(result) shouldBe 406
    }
  }
}


class TestPersonalIncomeRenewalSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "renewal Live" should {

    "process the renewal successfully" in new Success {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))

      status(result) shouldBe 200
    }

    "return 403 result when no tcr auth header has been supplied" in new Success {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithNoAuthHeader))

      status(result) shouldBe 403
    }

    "return bad result request when invalid json is submitted" in new Success {
      val result = await(controller.submitRenewal(nino)(renewalBadRequest))

      status(result) shouldBe 400
    }

    "Return 401 result when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))

      status(result) shouldBe 401
    }

    "return 406 result when the headers are invalid" in new Success {
      val result: Result = await(controller.submitRenewal(nino)(jsonRenewalRequestNoAcceptHeader))

      status(result) shouldBe 406
    }
  }

  "renewal sandbox" should {

    "return success response" in new SandboxSuccess {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))

      status(result) shouldBe 200
    }

    "return 403 result when no tcr auth header has been supplied" in new Success {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithNoAuthHeader))

      status(result) shouldBe 403
    }

    "return 406 result when the headers are invalid" in new SandboxSuccess {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestNoAcceptHeader))

      status(result) shouldBe 406
    }
  }
}


class TestPersonalIncomeRenewalSummarySpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "tax credits summary live" should {

    "process the request successfully" in new Success {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeaderAndAuthHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxRenewalSummary)
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }

    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequest))

      status(result) shouldBe 406
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    // TODO...add to all actions! This must be defined in an IT:test. Here as a reminder only to add!
    "return the sandbox result when the X-MOBILE-USER-ID is supplied" in new Success {
      val resource = findResource(s"/resources/taxcreditsummary/${nino.value}.json")
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeaderAndAuthHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(resource.get)
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }
  }

  "tax credits summary Sandbox" should {

    "return the summary response from a resource" in new SandboxSuccess {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200

      val resource = findResource(s"/resources/taxcreditsummary/${nino.value}.json")
      contentAsJson(result) shouldBe Json.parse(resource.get)

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

  }

}
