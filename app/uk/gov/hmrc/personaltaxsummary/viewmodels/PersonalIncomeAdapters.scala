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

object PersonalIncomeAdapters {

  import uk.gov.hmrc.personaltaxsummary.domain.MessageWrapper.applyForList3

  trait Converter[T, R] {
    def fromPTSModel(t: T): R
  }

  object PTSEstimatedIncomeViewModelConverter extends Converter[PTSEstimatedIncomeViewModel, EstimatedIncomeViewModel] {
    def fromPTSModel(pts: PTSEstimatedIncomeViewModel): EstimatedIncomeViewModel = {
      implicit val moneyParser = new MoneyParser

      EstimatedIncomeViewModel(
        pts.increasesTax,
        pts.incomeTaxEstimate,
        pts.incomeEstimate,
        pts.taxFreeEstimate,
        pts.taxRelief,
        pts.taxCodes,
        pts.potentialUnderpayment,
        pts.additionalTaxTableV2.map(ptsRow => AdditionalTaxRow(ptsRow.description, moneyParser.parse(ptsRow.amount))),
        moneyParser.parse(pts.additionalTaxTableTotal),
        pts.reductionsTable.map { case (description, amount, additionalInfo) => ReductionsRow(description, moneyParser.parse(amount), additionalInfo) },
        moneyParser.parse(pts.reductionsTableTotal),
        pts.graph,
        pts.hasChanges,
        pts.ukDividends,
        pts.taxBands,
        pts.incomeTaxReducedToZeroMessage,
        pts.nextYearTaxTotal,
        pts.hasPSA,
        pts.hasSSR
      )
    }
  }

  object PTSYourTaxableIncomeViewModelConverter extends Converter[PTSYourTaxableIncomeViewModel, YourTaxableIncomeViewModel] {
    def fromPTSModel(pts: PTSYourTaxableIncomeViewModel) = {
      new YourTaxableIncomeViewModel(
        pts.taxFreeAmount,
        pts.incomeTax,
        pts.income,
        pts.taxCodeList,
        pts.increasesTax,
        pts.employmentPension,
        applyForList3(pts.investmentIncomeData),
        pts.investmentIncomeTotal,
        applyForList3(pts.otherIncomeData),
        pts.otherIncomeTotal,
        pts.benefitsData,
        pts.benefitsTotal,
        applyForList3(pts.taxableBenefitsData),
        pts.taxableBenefitsTotal,
        pts.hasChanges
      )
    }
  }
}
