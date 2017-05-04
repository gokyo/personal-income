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

package uk.gov.hmrc.personalincome.controllers

import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.FakeApplication
import play.api.test.Helpers._
import uk.gov.hmrc.personalincome.domain.{ClaimantDetails, RenewalReference}
import uk.gov.hmrc.personalincome.services.SandboxPersonalIncomeService._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class TestPersonalIncomeSummarySpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "getSummary Live" should {

    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
      val result = await(controller.getSummary(ninoIncorrect, 90)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
    }

    "return 404 when summary returned is None" in new NotFound {

      val result: Result = await(controller.getSummary(nino, 90)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 404

      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value, "year" -> "90")
    }

    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.getSummary(nino, 90)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe noNinoOnAccont

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return unauthorized when authority record has a low CL" in new AuthWithLowCL {
      val result = await(controller.getSummary(nino, 90)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe lowCl

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.getSummary(nino, 90)(emptyRequest))

      status(result) shouldBe 406
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }
  }
}

class TestPersonalIncomeRenewalAuthenticateSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "authenticate Live" should {

    "process the authentication successfully" in new Success {
      val result = await(controller.getRenewalAuthentication(nino, renewalReference)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(tcrAuthToken)
    }

    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
      val result = await(controller.getRenewalAuthentication(ninoIncorrect, renewalReference)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
    }

    "process the authentication successfully when journeyId is supplied" in new Success {
      val result = await(controller.getRenewalAuthentication(nino, renewalReference, Some(journeyId))(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(tcrAuthToken)
    }

    "return 404 response when hod returns 4xx status" in new Ntc400Result {
      val result = await(controller.getRenewalAuthentication(nino, renewalReferenceNines)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 404
    }

    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.getRenewalAuthentication(nino, renewalReference)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe noNinoOnAccont

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return unauthorized when authority record has a low CL" in new AuthWithLowCL {
      val result = await(controller.getRenewalAuthentication(nino, renewalReference)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe lowCl

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.getRenewalAuthentication(nino, renewalReference)(emptyRequest))

      status(result) shouldBe 406
    }

  }
}


class TestPersonalIncomeRenewalClaimantDetailsSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "requesting claimant details Live" should {

    "return claimant details successfully" in new Success {
      val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimentDetails.copy(mainApplicantNino = "true"))
      result.header.headers.get("Cache-Control") shouldBe None
    }

    "return claimant details successfully when NINO does not match mainApplicantNino" in new Success {
      override lazy val claimentDetails = ClaimantDetails(false, 1, "r", "CS777100A", None, false, "some-app-id")

      val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimentDetails.copy(mainApplicantNino = "false"))
      result.header.headers.get("Cache-Control") shouldBe None
    }

    "return claimant claims successfully" in new Success {
      val result = await(controller.claimantDetails(nino, None, Some("claims"))(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJson)
      result.header.headers.get("Cache-Control") shouldBe Some("max-age=1800")
    }

    "return claimant claims successfully and drop invalid dates from the response" in new SuccessWithInvalidDates {
      val result = await(controller.claimantDetails(nino, None, Some("claims"))(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJsonWithInvalidDates)
    }

    "return 404 when no claims matched the supplied nino" in new NotFoundClaimant {
      val result = await(controller.claimantDetails(nino, None, Some("claims"))(emptyRequestWithAcceptHeader))

      status(result) shouldBe 404
    }

    "return 403 when no tcrAuthHeader is supplied to claimant details API" in new NotFoundClaimant {
      val result = await(controller.claimantDetails(nino, None, None)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.parse("""{"code":"NTC_RENEWAL_AUTH_ERROR","message":"No auth header supplied in http request"}""")
    }

    "return 403 when tcrAuthHeader is supplied to claims API" in new NotFoundClaimant {
      val result = await(controller.claimantDetails(nino, None, Some("claims"))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.parse("""{"code":"NTC_RENEWAL_AUTH_ERROR","message":"Auth header is not required in the request"}""")
    }

    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
      val result = await(controller.claimantDetails(ninoIncorrect)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 401
    }

    "return the summary successfully when journeyId is supplied" in new Success {
      val result = await(controller.claimantDetails(nino, Some(journeyId))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimentDetails.copy(mainApplicantNino = "true"))
    }

    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe noNinoOnAccont

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return unauthorized when authority record has a low CL" in new AuthWithLowCL {
      val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe lowCl

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return 403 response when the tcr auth header is not supplied in the request" in new Success {
      val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.toJson(ErrorNoAuthToken)
    }

    "return status code 406 when the Accept header is invalid" in new Success {
      val result = await(controller.claimantDetails(nino)(emptyRequest))

      status(result) shouldBe 406
    }

  }

  "claimant details Sandbox" should {

    "return claimant details successfully when an unknown bar code reference is supplied" in new SandboxSuccess {
      val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(RenewalReference("888888888888888"))))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimentDetails.copy(mainApplicantNino = "false", hasPartner = false, renewalFormType = "r"))
    }

    "return claimant claims successfully" in new SandboxSuccess {

      val result = await(controller.claimantDetails(nino, None, Some("claims"))(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJson)
    }

    "return claimant details successfully when a known bar code reference is supplied" in new SandboxSuccess {

      case class TestData(barcode: String, renewalFormType: String, hasPartner: Boolean = false)

      val testData = Seq(TestData("111111111111111", "r"), TestData("222222222222222", "d"), TestData("333333333333333", "d2"), TestData("444444444444444", "d", hasPartner = true), TestData("555555555555555", "d2", true))

      testData.map(item => {
        val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(RenewalReference(item.barcode))))
        status(result) shouldBe 200
        contentAsJson(result) shouldBe Json.toJson(claimentDetails.copy(mainApplicantNino = "true", hasPartner = item.hasPartner, renewalFormType = item.renewalFormType))
      })

    }

    "return 403 response when the tcr auth header is not supplied in the request" in new SandboxSuccess {
      val result = await(controller.claimantDetails(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 403

      contentAsJson(result) shouldBe Json.toJson(ErrorNoAuthToken)
    }

    "return status code 406 when the Accept header is invalid" in new SandboxSuccess {
      val result = await(controller.claimantDetails(nino)(emptyRequest))

      status(result) shouldBe 406
    }
  }
}


class TestPersonalIncomeRenewalSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  "renewal Live" should {

    "process the renewal successfully if renewals are enabled" in new Success {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))

      ntcConnector.renewalCount shouldBe 1
      status(result) shouldBe 200
    }

    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
      val result = await(controller.submitRenewal(ninoIncorrect)(jsonRenewalRequestWithAuthHeader))

      status(result) shouldBe 401
    }

    "process the renewal successfully if renewals are disabled" in new SuccessRenewalDisabled {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))

      ntcConnector.renewalCount shouldBe 0
      status(result) shouldBe 200
    }

    "process returns a 200 successfully when journeyId is supplied" in new Success {
      val result = await(controller.submitRenewal(nino, Some(journeyId))(jsonRenewalRequestWithAuthHeader))

      ntcConnector.renewalCount shouldBe 1
      status(result) shouldBe 200
    }

    "process returns a 200 when journeyId is supplied and Renewals are disabled" in new SuccessRenewalDisabled {
      val result = await(controller.submitRenewal(nino, Some(journeyId))(jsonRenewalRequestWithAuthHeader))

      ntcConnector.renewalCount shouldBe 0
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
      contentAsJson(result) shouldBe noNinoOnAccont

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return unauthorized when authority record has a low CL" in new AuthWithLowCL {
      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe lowCl

      testPersonalIncomeService.saveDetails shouldBe Map.empty
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

    "process the request successfully and filter children older than 20 and where deceased flags are active" in new Success {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxRenewalSummaryWithoutChildrenOverAge20)
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }

    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
      val result = await(controller.taxCreditsSummary(ninoIncorrect)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 401
    }

    "return 429 HTTP status when retrieval of children returns 503" in new Generate_503 {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 429
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }

    "return the summary successfully when journeyId is supplied" in new Success {
      val result = await(controller.taxCreditsSummary(nino, Some(journeyId))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxRenewalSummaryWithoutChildrenOverAge20)
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }

    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe noNinoOnAccont
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return unauthorized when authority record has a low CL" in new AuthWithLowCL {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe lowCl

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.taxCreditsSummary(nino)(emptyRequest))

      status(result) shouldBe 406
      testPersonalIncomeService.saveDetails shouldBe Map.empty
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


class TestExclusionsServiceSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

  "tax exclusions service" should {

    "process the request for get tax credit exclusion successfully" in new Success {
      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe exclusionResult
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }

    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
      val result = await(controller.getTaxCreditExclusion(ninoIncorrect)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 401
    }

    "return 429 HTTP status when get tax credit exclusion returns 503" in new Generate_503 {
      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 429
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }

    "return the tax credit exclusion successfully when journeyId is supplied" in new Success {
      val result = await(controller.getTaxCreditExclusion(nino, Some(journeyId))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe exclusionResult
      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
    }

    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe noNinoOnAccont
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return unauthorized when authority record has a low Confidence Level" in new AuthWithLowCL {
      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeader))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe lowCl

      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequest))

      status(result) shouldBe 406
      testPersonalIncomeService.saveDetails shouldBe Map.empty
    }
  }
}
