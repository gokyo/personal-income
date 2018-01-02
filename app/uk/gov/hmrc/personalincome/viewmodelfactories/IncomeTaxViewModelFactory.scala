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

import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.domain.TaxSummaryDetails
import uk.gov.hmrc.personaltaxsummary.viewmodels.IncomeTaxViewModel

object IncomeTaxViewModelFactory extends ViewModelFactory[IncomeTaxViewModel] {

  override def createObject(nino: Nino, details: TaxSummaryDetails): IncomeTaxViewModel = {
    val totalLiability = details.totalLiability

    val incomeTax = totalLiability.map(_.totalTax).getOrElse(BigDecimal(0))
    val taxableIncome = details.increasesTax.map(_.total).getOrElse(BigDecimal(0))
    val taxFreeAmount = details.decreasesTax.map(_.total).getOrElse(BigDecimal(0))
    val personalAllowance = details.decreasesTax.flatMap(_.personalAllowance).getOrElse(BigDecimal(0))
    val hasTamc = details.decreasesTax.flatMap(_.paTransferredAmount).isDefined || details.decreasesTax.flatMap(_.paReceivedAmount).isDefined

    val taxCodeEmploymentList = {
      details.taxCodeDetails.flatMap(_.employment) match {
        case Some(list) => for (e <- list) yield e.taxCode.get
        case _ => List[String]()
      }
    }

    IncomeTaxViewModel(
      incomeTax,
      taxableIncome,
      taxFreeAmount,
      personalAllowance,
      hasTamc,
      taxCodeEmploymentList,
      details.cyPlusOneChange.isDefined,
      isSimpleTaxUser(taxCodeEmploymentList)
    )
  }

  private def isSimpleTaxUser(taxCodeList: List[String]): Boolean = {
    taxCodeList.length == 1 && taxCodeList.head == "1100L"
  }

}
