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

package uk.gov.hmrc.personalincome.viewmodelfactories

import play.api.libs.json.Json
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.personalincome.utils.TaxSummaryHelper
import uk.gov.hmrc.personaltaxsummary.domain.{EstimatedIncomeWrapper, GateKeeperDetails, TaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.viewmodels.{PTSEstimatedIncomeViewModel, PTSYourTaxableIncomeViewModel}
import uk.gov.hmrc.personaltaxsummary.viewmodels.PersonalIncomeAdapters._


object TaxSummaryContainerFactory {

  def buildTaxSummaryContainer(nino: Nino, details: TaxSummaryDetails, estimatedIncome: PTSEstimatedIncomeViewModel, taxableIncome: PTSYourTaxableIncomeViewModel): TaxSummaryContainer = {

    val incomeTax = IncomeTaxViewModelFactory.createObject(nino, details)

    val taxSummaryDetailsResponse = Json.toJson(details).as[TaxSummaryDetailsResponse]
    val estimatedIncomeModel  = PTSEstimatedIncomeViewModelConverter.fromPTSModel(estimatedIncome)
    val taxableIncomeModel = PTSYourTaxableIncomeViewModelConverter.fromPTSModel(taxableIncome)


    if (!isGateKeepered(taxSummaryDetailsResponse)) {
      val potentialUnderPayment = getPotentialUnderpayment(taxSummaryDetailsResponse)

      val wrappedEstimatedIncome = EstimatedIncomeWrapper(estimatedIncomeModel, potentialUnderPayment)
      TaxSummaryContainer(
        taxSummaryDetailsResponse,
        incomeTax,
        Some(wrappedEstimatedIncome),
        Some(taxableIncomeModel),
        None
      )
    } else {
      val gatekeeper = GateKeeperDetails(TotalLiability(totalTax = 0), DecreasesTax(total = 0), increasesTax = IncreasesTax(total = 0))
      TaxSummaryContainer(
        taxSummaryDetailsResponse,
        incomeTax,
        None,
        None,
        Option(gatekeeper)
      )
    }
  }

  def isGateKeepered(taxSummary: TaxSummaryDetailsResponse): Boolean = {
    taxSummary.gateKeeper.exists(_.gateKeepered)
  }

  def getPotentialUnderpayment(taxDetails: TaxSummaryDetailsResponse): Option[BigDecimal] = {
    val incomesWithUnderpayment = taxDetails.increasesTax
      .flatMap(_.incomes.map(incomes =>
        TaxSummaryHelper.sortedTaxableIncomes(incomes.taxCodeIncomes).filter(_.tax.totalInYearAdjustment.isDefined)))
      .getOrElse(Nil)

    incomesWithUnderpayment.foldLeft(BigDecimal(0))((total, income) =>
      income.tax.totalInYearAdjustment.getOrElse(BigDecimal(0)) + total)
    match {
      case x if x > 0 => Some(x)
      case _ => None
    }
  }
}




