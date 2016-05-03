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

package uk.gov.hmrc.personalincome.connectors

import com.typesafe.config.{Config, ConfigFactory}
import play.api.test.FakeApplication
import uk.gov.hmrc.personalincome.domain.{TaxCreditsSubmissionControl, TaxCreditsSubmissionControlConfig}
import uk.gov.hmrc.play.test.UnitSpec

class TaxCreditsSubmissionControlSpec extends UnitSpec {

  val specConfig = ConfigFactory.parseString(
    """microservice {
      |  services {
      |    ntc {
      |      submission {
      |        shutter = false
      |        startDate = "2016-04-01T00:00:00.000Z"
      |        endDate = "2016-07-31T23:59:59.999Z"
      |      }
      |    }
      |  }
      |}
      | """.stripMargin)

  lazy val taxCreditsSubmissionControlConfig = new TaxCreditsSubmissionControlConfig {
    override lazy val config: Config = specConfig
  }

  "TaxCreditsSubmissionControl" should {
    "test config initialise" in {
      val sc = taxCreditsSubmissionControlConfig.submissionControl
      sc.shutter shouldBe false

      val start = sc.startDate
      start.getDayOfMonth shouldBe 1
      start.getMonthOfYear shouldBe 4
      start.getHourOfDay shouldBe 0

      val end = sc.endDate
      end.getDayOfMonth shouldBe 31
      end.getMonthOfYear shouldBe 7
      end.getHourOfDay shouldBe 23
      end.getSecondOfMinute shouldBe 59
    }

    "application initialisation" in {

      lazy val fakeApplication = FakeApplication()

      val sc = TaxCreditsSubmissionControl.submissionControl
      sc.shutter shouldBe false

      val start = sc.startDate
      start.getDayOfMonth shouldBe 1
      start.getMonthOfYear shouldBe 4
      start.getHourOfDay shouldBe 0

      val end = sc.endDate
      end.getDayOfMonth shouldBe 31
      end.getMonthOfYear shouldBe 7
      end.getHourOfDay shouldBe 23
      end.getSecondOfMinute shouldBe 59
    }
  }

}
