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
import uk.gov.hmrc.model.{TaxBand, TaxComponent}
import uk.gov.hmrc.personaltaxsummary.domain.MessageWrapper

case class EstimatedIncomeViewModel(
                            increasesTax: Boolean = false,
                            incomeTaxEstimate: BigDecimal = 0,
                            incomeEstimate: BigDecimal = 0,
                            taxFreeEstimate: BigDecimal = 0,
                            taxRelief: Boolean = false,
                            taxCodes: List[String] = List(),
                            potentialUnderpayment: Boolean = false,
                            additionalTaxTable: List[MessageWrapper] = List(),
                            additionalTaxTableTotal: String = "",
                            reductionsTable: List[MessageWrapper] = List(),
                            reductionsTableTotal: String = "",
                            graph: BandedGraph,
                            hasChanges: Boolean = false,
                            ukDividends: Option[TaxComponent],
                            taxBands: Option[List[TaxBand]],
                            incomeTaxReducedToZeroMessage: Option[String]
                          )

case class BandedGraph(
                        id: String,
                        bands: List[Band] = List(),
                        minBand: BigDecimal = 0,
                        nextBand: BigDecimal = 0,
                        incomeTotal: BigDecimal = 0,
                        incomeAsPercentage: BigDecimal = 0,
                        taxTotal: BigDecimal = 0
                      )

case class Band(
                 colour: String,
                 barPercentage: BigDecimal = 0,
                 tablePercentage: String = "0",
                 income: BigDecimal = 0,
                 tax: BigDecimal = 0
               )

object Band {
  implicit val format = Json.format[Band]
}

object BandedGraph {
  implicit val format = Json.format[BandedGraph]
}

object EstimatedIncomeViewModel {
  implicit val format = Json.format[EstimatedIncomeViewModel]
}

