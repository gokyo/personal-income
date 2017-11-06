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

package uk.gov.hmrc.personaltaxsummary.viewmodels

import org.scalatest.{Matchers, WordSpec}
import uk.gov.hmrc.personaltaxsummary.domain.MessageWrapper

class PersonalIncomeAdaptersSpec extends WordSpec with Matchers {

  private val defaultBandedGraph = BandedGraph(id = "id")

  private val defaultPTSEstimatedIncomeViewModel = PTSEstimatedIncomeViewModel(
    graph = defaultBandedGraph,
    newGraph = BandedGraph(id = "id"),
    ukDividends = None,
    taxBands = None,
    incomeTaxReducedToZeroMessage = None)

  "PTSEstimatedIncomeViewModelConverter" should {

    "convert additionalTaxTableV2 to MessageWrappers" in {
      val ptsModel = defaultPTSEstimatedIncomeViewModel.copy(additionalTaxTableV2 = List(
        PTSAdditionalTaxRow(description = "Description 1", amount = "100.00"),
        PTSAdditionalTaxRow(description = "Description 2", amount = "2,000.00"),
        PTSAdditionalTaxRow(description = "Description 3", amount = "3,000.00")
      ))

      val model = PersonalIncomeAdapters.PTSEstimatedIncomeViewModelConverter.fromPTSModel(ptsModel)
      model.additionalTaxTable shouldBe List(
        MessageWrapper("Description 1", "100.00"),
        MessageWrapper("Description 2", "2,000.00"),
        MessageWrapper("Description 3", "3,000.00")
      )
    }
  }
}
