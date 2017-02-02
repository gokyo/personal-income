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

import play.api.libs.json.Json
import uk.gov.hmrc.model.{IncreasesTax, TaxCodeIncomes}
import uk.gov.hmrc.personaltaxsummary.domain.{BenefitsDataWrapper, MessageWrapper}

case class YourTaxableIncomeViewModel(
                                       taxFreeAmount: BigDecimal,
                                       incomeTax: BigDecimal,
                                       income: BigDecimal,
                                       taxCodeList: List[String],
                                       increasesTax: Option[IncreasesTax] = None,
                                       employmentPension: EmploymentPension,
                                       investmentIncomeData: List[MessageWrapper] = List(),
                                       investmentIncomeTotal: BigDecimal,
                                       otherIncomeData: List[MessageWrapper] = List(),
                                       otherIncomeTotal: BigDecimal,
                                       benefitsData: List[BenefitsDataWrapper] = List(),
                                       benefitsTotal: BigDecimal,
                                       taxableBenefitsData: List[MessageWrapper] = List(),
                                       taxableBenefitsTotal: BigDecimal,
                                       hasChanges: Boolean = false
                                     )

case class EmploymentPension(
                              taxCodeIncomes: Option[TaxCodeIncomes],
                              totalEmploymentPensionAmt: BigDecimal = BigDecimal(0),
                              hasEmployment: Boolean = false,
                              isOccupationalPension: Boolean = false
                            )

object EmploymentPension {
  implicit val format = Json.format[EmploymentPension]
}

object YourTaxableIncomeViewModel {
  implicit val format = Json.format[YourTaxableIncomeViewModel]
}
