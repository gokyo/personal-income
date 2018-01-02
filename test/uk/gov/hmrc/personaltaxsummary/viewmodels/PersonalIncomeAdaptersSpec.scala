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

package uk.gov.hmrc.personaltaxsummary.viewmodels

import java.text.ParseException

import org.scalatest.{Matchers, WordSpec}

class PersonalIncomeAdaptersSpec extends WordSpec with Matchers {

  private val defaultBandedGraph = BandedGraph(id = "id")

  private val defaultPTSEstimatedIncomeViewModel = PTSEstimatedIncomeViewModel(
    graph = defaultBandedGraph,
    newGraph = BandedGraph(id = "id"),
    ukDividends = None,
    taxBands = None,
    incomeTaxReducedToZeroMessage = None)

  "PTSEstimatedIncomeViewModelConverter" should {

    "convert additionalTaxTableV2 to AdditionalTaxRows" in {
      val ptsModel = defaultPTSEstimatedIncomeViewModel.copy(additionalTaxTableV2 = List(
        PTSAdditionalTaxRow(description = "Description 1", amount = "100.00"),
        PTSAdditionalTaxRow(description = "Description 2", amount = "2,000.00"),
        PTSAdditionalTaxRow(description = "Description 3", amount = "3,000.12")
      ))

      val model = PersonalIncomeAdapters.PTSEstimatedIncomeViewModelConverter.fromPTSModel(ptsModel)
      model.additionalTaxTable shouldBe List(
        AdditionalTaxRow("Description 1", BigDecimal(100)),
        AdditionalTaxRow("Description 2", BigDecimal(2000.00)),
        AdditionalTaxRow("Description 3", BigDecimal("3000.12"))
      )
    }

    "throw an exception when the number in an additionalTaxTableV2 row can't be parsed" in {
      intercept[ParseException] {
        val ptsModel = defaultPTSEstimatedIncomeViewModel.copy(additionalTaxTableV2 = List(
          PTSAdditionalTaxRow(description = "Description 1", amount = "cannot be parsed as a number")
        ))

        PersonalIncomeAdapters.PTSEstimatedIncomeViewModelConverter.fromPTSModel(ptsModel)
      }
    }

    "convert reductionsTable to ReductionsRows" in {
      val ptsModel = defaultPTSEstimatedIncomeViewModel.copy(reductionsTable = List(
        ("Description 1", "100.00",   "additional info 1"),
        ("Description 2", "2,000.00", "additional info 2"),
        ("Description 3", "3,000.12", "additional info 3")
      ))

      val model = PersonalIncomeAdapters.PTSEstimatedIncomeViewModelConverter.fromPTSModel(ptsModel)
      model.reductionsTable shouldBe List(
        ReductionsRow(description = "Description 1", amount = BigDecimal(100), additionalInfo = "additional info 1"),
        ReductionsRow(description = "Description 2", amount = BigDecimal(2000.00), additionalInfo = "additional info 2"),
        ReductionsRow(description = "Description 3", amount = BigDecimal("3000.12"), additionalInfo = "additional info 3")
      )
    }

    "throw an exception when the number in an reductionsTable row can't be parsed" in {
      intercept[ParseException] {
        val ptsModel = defaultPTSEstimatedIncomeViewModel.copy(reductionsTable = List(
          ("Description 1", "cannot be parsed as a number", "additional info 1")
        ))

        PersonalIncomeAdapters.PTSEstimatedIncomeViewModelConverter.fromPTSModel(ptsModel)
      }
    }
  }
}
