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
import play.api.libs.json.Json.parse
import play.api.mvc.Result
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class ServiceStateControllerSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {


  "taxCreditsSubmissionState Live" should {

    "return the submission state" in new TaxCreditRenewalsSubmissionPeriod {

      val result: Result = await(controller.taxCreditsSubmissionState()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe parse("""{"renewalsSubmissionShuttered":false,"inSubmitRenewalsPeriod":true,"inViewRenewalsPeriod":true}""")
    }
  }

  "taxCreditsSubmissionState Sandbox" should {

    "return the submission state" in new SandboxServiceStateSuccess {

      val result: Result = await(controller.taxCreditsSubmissionState()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe parse("""{"renewalsSubmissionShuttered":false,"inSubmitRenewalsPeriod":true,"inViewRenewalsPeriod":true}""")
    }
  }

  "taxCreditsSubmissionStateEnabled Live" should {

    "enable renewals submission when renewalsSubmissionShuttered is OFF during the Submission Period" in new TaxCreditRenewalsSubmissionPeriod {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe parse("""{"enableRenewals":true,"enableViewRenewals":true}""")
    }

    "disable renewals submission when renewalsSubmissionShuttered is ON during the Submission Period" in new TaxCreditRenewalsSubmissionPeriodShuttered {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe parse("""{"enableRenewals":false,"enableViewRenewals":true}""")
    }

    "disable renewals submission during the view-only period" in new TaxCreditRenewalsViewOnlyPeriod {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe parse("""{"enableRenewals":false,"enableViewRenewals":true}""")
    }

    "disable renewals submission and viewing during the closed period" in new TaxCreditRenewalsClosedPeriod {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe parse("""{"enableRenewals":false,"enableViewRenewals":false}""")
    }
  }

  "taxCreditsSubmissionStateEnabled Sandbox" should {

    "enable renewals submission and viewing" in new SandboxServiceStateSuccess {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe parse("""{"enableRenewals":true,"enableViewRenewals":true}""")
    }
  }

}
