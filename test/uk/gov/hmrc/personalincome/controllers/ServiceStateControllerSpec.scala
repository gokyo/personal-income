/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.personalincome.domain.{TaxCreditsControl, TaxCreditsRenewalsState, TaxCreditsSubmissions}
import uk.gov.hmrc.play.test.WithFakeApplication

class ServiceStateControllerSpec extends TestSetup with WithFakeApplication {

  class TestServiceStateController(taxCreditsSubmissions: TaxCreditsSubmissions = new TaxCreditsSubmissions(false, true, true)) extends ServiceStateController {
    override val taxCreditsSubmissionControlConfig: TaxCreditsControl = new TaxCreditsControl {
      override def toTaxCreditsSubmissions: TaxCreditsSubmissions = taxCreditsSubmissions
      override def toTaxCreditsRenewalsState: TaxCreditsRenewalsState = taxCreditsSubmissions.toTaxCreditsRenewalsState
    }
  }

  "taxCreditsSubmissionState Live" should {
    "return the submission state" in {
      val controller = new TestServiceStateController()
      val result = await(controller.taxCreditsSubmissionState().apply(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse("""{"submissionShuttered":false,"inSubmitRenewalsPeriod":true,"inViewRenewalsPeriod":true}""")
    }
  }

  "taxCreditsSubmissionState Sandbox" should {
    "return the submission state" in {
      val controller = new SandboxServiceStateController()
      val result = await(controller.taxCreditsSubmissionState().apply(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse("""{"submissionShuttered":false,"inSubmitRenewalsPeriod":true,"inViewRenewalsPeriod":true}""")
    }
  }

  "taxCreditsSubmissionStateEnabled Live" should {
    "enable renewals submission when submissionShuttered is OFF during the Submission Period" in {
      val controller = new TestServiceStateController()
      val result = await(controller.taxCreditsSubmissionStateEnabled()(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse("""{"submissionsState":"open"}""")
    }

    "shutter renewals submission when submissionShuttered is ON during the Submission Period" in {
      val controller = new TestServiceStateController(new TaxCreditsSubmissions(true, true, true))
      val result = await(controller.taxCreditsSubmissionStateEnabled()(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse("""{"submissionsState":"shuttered"}""")
    }

    "disable renewals submission during the check_status_only period" in {
      val controller = new TestServiceStateController(new TaxCreditsSubmissions(true, false, true))
      val result = await(controller.taxCreditsSubmissionStateEnabled()(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse("""{"submissionsState":"check_status_only"}""")
    }

    "disable renewals submission and viewing during the closed period" in {
      val controller = new TestServiceStateController(new TaxCreditsSubmissions(true, false, false))
      val result = await(controller.taxCreditsSubmissionStateEnabled()(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse("""{"submissionsState":"closed"}""")
    }
  }

  "taxCreditsSubmissionStateEnabled Sandbox" should {
    "enable renewals submission and viewing" in {
      val controller = new SandboxServiceStateController()
      val result = await(controller.taxCreditsSubmissionStateEnabled().apply(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse("""{"submissionsState":"open"}""")
    }
  }
}