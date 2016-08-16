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

package uk.gov.hmrc.personalincome.controllers

import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.mvc.Result
import play.api.test.Helpers._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class ServiceStateControllerSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {


  "taxCreditsSubmissionState Live" should {

    "return the submission state" in new ServiceStateSuccess {

      val result: Result = await(controller.taxCreditsSubmissionState()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe Json.parse("""{"shuttered":false,"inSubmissionPeriod":true}""")
    }
  }

  "taxCreditsSubmissionState Sandbox" should {

    "return the submission state" in new SandboxServiceStateSuccess {

      val result: Result = await(controller.taxCreditsSubmissionState()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe Json.parse("""{"shuttered":false,"inSubmissionPeriod":true}""")
    }
  }

  "taxCreditsSubmissionStateEnabled Live" should {

    "return the submission state true when Shuttering is OFF and inSubmission Period is true" in new ServiceStateSuccess {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe Json.parse("""{"submissionState":true}""")
    }

    "return the submission state false when Shuttering is ON and inSubmission Period is true" in new ServiceStateSuccessShuttered {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe Json.parse("""{"submissionState":false}""")
    }

    "return the submission state false when Shuttering is OFF and inSubmission Period is false" in new ServiceStateNotInSubmissionPeriod {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe Json.parse("""{"submissionState":false}""")
    }
  }

  "taxCreditsSubmissionStateEnabled Sandbox" should {

    "return the submission state true when Shuttering is off and inSubmission Period is true" in new SandboxServiceStateSuccess {

      val result: Result = await(controller.taxCreditsSubmissionStateEnabled()(emptyRequestWithAcceptHeader))

      status(result) shouldBe 200
      print(contentAsJson(result))
      contentAsJson(result) shouldBe Json.parse("""{"submissionState":true}""")
    }
  }

}
