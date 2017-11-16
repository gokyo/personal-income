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

package uk.gov.hmrc.personalincome

import play.api.libs.json.{JsSuccess, Json}
import uk.gov.hmrc.personalincome.domain.{Tax, TotalLiability}
import uk.gov.hmrc.play.test.UnitSpec

class TaxSummaryModelSpec extends UnitSpec {

  "TaxSummaryModel" should {
    "parse TotalLiability" in {

      val expectedResponse =  """
                                |{
                                |  "totalTax": 1234.99,
                                |  "underpaymentPreviousYear": 0.00,
                                |  "inYearAdjustment": 10.00,
                                |  "outstandingDebt": 0.00,
                                |  "childBenefitTaxDue": 5432.10,
                                |  "taxOnBankBSInterest": 0.00
                                |}
                              """.stripMargin

      val response = Json.parse(expectedResponse).validate[TotalLiability]
      val totalLiability = response match {
        case success : JsSuccess[TotalLiability] ⇒ success.get
        case _ ⇒ fail("Failed to parse TotalLiability")
      }

      totalLiability.nonCodedIncome shouldBe None
      totalLiability.totalTax shouldBe BigDecimal(1234.99)
      totalLiability.underpaymentPreviousYear shouldBe BigDecimal(0.00)
      totalLiability.inYearAdjustment shouldBe Some(BigDecimal(10.00))
      totalLiability.outstandingDebt shouldBe BigDecimal(0.00)
      totalLiability.childBenefitTaxDue shouldBe BigDecimal(5432.10)
      totalLiability.taxOnBankBSInterest shouldBe Some(BigDecimal(0.00))
      totalLiability.taxCreditOnUKDividends shouldBe None
      totalLiability.taxCreditOnForeignInterest shouldBe None
      totalLiability.taxCreditOnForeignIncomeDividends shouldBe None
      totalLiability.liabilityReductions shouldBe None
      totalLiability.liabilityAdditions shouldBe None
    }

    "parse Tax" in {
      val expectedResponse =  """
                                |{
                                |  "totalIncome": 108.10,
                                |  "totalTaxableIncome": 100.00,
                                |  "totalTax": 10.00,
                                |  "totalInYearAdjustment": 0.00,
                                |  "inYearAdjustmentIntoCY": 32.10,
                                |  "inYearAdjustmentIntoCYPlusOne": 0.00,
                                |  "inYearAdjustmentFromPreviousYear": 0.00,
                                |  "actualTaxDueAssumingBasicRateAlreadyPaid": 33.33,
                                |  "actualTaxDueAssumingAllAtBasicRate": 66.66
                                |}
                              """.stripMargin
      val response = Json.parse(expectedResponse).validate[Tax]
      val tax = response match {
        case success : JsSuccess[Tax] ⇒ success.get
        case _ ⇒ fail("Failed to parse Tax")
      }

      tax.totalIncome shouldBe Some(BigDecimal(108.10))
      tax.totalTaxableIncome shouldBe Some(BigDecimal(100.00))
      tax.totalTax shouldBe Some(BigDecimal(10.00))
      tax.totalInYearAdjustment shouldBe Some(BigDecimal(0.00))
      tax.inYearAdjustmentIntoCY shouldBe Some(BigDecimal(32.10))
      tax.inYearAdjustmentIntoCYPlusOne shouldBe Some(BigDecimal(0.00))
      tax.inYearAdjustmentFromPreviousYear shouldBe Some(BigDecimal(0.00))
      tax.taxBands shouldBe None
      tax.allowReliefDeducts shouldBe None
      tax.actualTaxDueAssumingBasicRateAlreadyPaid shouldBe Some(BigDecimal(33.33))
      tax.actualTaxDueAssumingAllAtBasicRate shouldBe Some(BigDecimal(66.66))
    }
  }
}
