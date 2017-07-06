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

package uk.gov.hmrc.personalincome.services

import uk.gov.hmrc.personalincome.config.AppContext
import uk.gov.hmrc.personalincome.domain.{Applicant, Claim, Household, Renewal}
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class RenewalStatusSpec extends UnitSpec with WithFakeApplication {

  val renewalStatus = new RenewalStatus {
    override def renewalStatusTransform: Option[List[AppContext.RenewalStatusTransform]] = None
  }

  val household = Household("1010101", "1234", Applicant("NINO", "MR", "TOM", None, "SMITH"), None, None, None)
  def claim(renewalStatus: Some[String] = Some("SUPERCEDED")) = Claim(household, Renewal(None, None, renewalStatus, None, None))

  "Renewal status" should {
    "transform a status" in {
      renewalStatus.resolveStatus(claim(Some("SUPERCEDED"))) shouldBe "SUBMITTED_AND_PROCESSING"
    }

    "handle unknown status as NOT_SUBMITTED" in {
      renewalStatus.resolveStatus(claim(Some("foo"))) shouldBe "NOT_SUBMITTED"
    }

    "handles awaiting barcode" in {
      val noBarcodeClaim = claim().copy(household = household.copy(barcodeReference = "000000000000000"))

      renewalStatus.resolveStatus(noBarcodeClaim) shouldBe "AWAITING_BARCODE"
    }

    "transform case insensitively" in {
      renewalStatus.resolveStatus(claim(Some("SuPeRcEdEd"))) shouldBe "SUBMITTED_AND_PROCESSING"
    }

    "transform ignoring whitespace" in {
      renewalStatus.resolveStatus(claim(Some("   SUPERCEDED   "))) shouldBe "SUBMITTED_AND_PROCESSING"
    }
  }
}
