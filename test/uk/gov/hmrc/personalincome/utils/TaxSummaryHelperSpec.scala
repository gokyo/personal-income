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

package uk.gov.hmrc.personalincome.utils

import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.test.FakeApplication
import uk.gov.hmrc.personalincome.domain.EstimatedIncomePageVM
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.{WithFakeApplication, UnitSpec}
import uk.gov.hmrc.personalincome.controllers.StubApplicationConfiguration

class TaxSummaryHelperSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {

override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)

  val nino = Nino("CS700100A")

  "Data mapping for tax summary objects" should {

    "successfully create a BaseViewModel view from TaxSummaryDetails object and serialize to json" in {
      val res: BaseViewModel = BaseViewModelVM.createObject(nino, Data.getEverything)

      Json.toJson(res) shouldBe Json.toJson(Data.getBaseLevelView)
    }

    "successfully create EstimatedIncome view from TaxSummaryDetails object and serialize to json" in {
      val res: EstimatedIncome = EstimatedIncomePageVM.createObject(nino, Data.getEverything)

      Json.toJson(res) shouldBe Json.toJson(Data.getEstimatedIncome)
    }

    "successfully create TaxableIncome view from TaxSummaryDetails object and serialize to json" in {
      val res: TaxableIncome = YourTaxableIncomePageVM.createObject(nino, Data.getEverything)

      Json.toJson(res) shouldBe Json.toJson(Data.getTaxableIncome)
    }

    "successfully create a TaxSummaryContainer object and serialize to json" in {
      val taxSummaryDetails = Data.getEverything
      val base: BaseViewModel = BaseViewModelVM.createObject(nino, taxSummaryDetails)
      val estimated: EstimatedIncome = EstimatedIncomePageVM.createObject(nino, taxSummaryDetails)
      val taxable = YourTaxableIncomePageVM.createObject(nino, taxSummaryDetails)

      val taxSummaryContainer = TaxSummaryContainer(taxSummaryDetails, base, Some(EstimatedIncomeWrapper(estimated, Some(10))), Some(taxable), None)

      Json.toJson(taxSummaryContainer) shouldBe Json.toJson(Data.getTaxSummaryContainer)
    }


    "verify handling of errors during creation of domain objects from partial TaxSummaryDetails" in {

      val taxSummaryDetails = TaxSummaryDetails(nino.value, 1)

      BaseViewModelVM.createObject(nino, taxSummaryDetails)

      intercept[java.util.NoSuchElementException] {
        EstimatedIncomePageVM.createObject(nino, taxSummaryDetails)
      }

      YourTaxableIncomePageVM.createObject(nino, taxSummaryDetails)
    }

  }
}
