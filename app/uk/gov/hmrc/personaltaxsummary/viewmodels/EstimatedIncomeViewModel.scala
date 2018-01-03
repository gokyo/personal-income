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

import play.api.libs.json.Json
import uk.gov.hmrc.personalincome.domain._

case class PTSEstimatedIncomeViewModel(
                                        increasesTax: Boolean = false,
                                        incomeTaxEstimate: BigDecimal = 0,
                                        incomeEstimate: BigDecimal = 0,
                                        taxFreeEstimate: BigDecimal = 0,
                                        taxRelief: Boolean = false,
                                        taxCodes: List[String] = List(),
                                        potentialUnderpayment:Boolean = false,
                                        additionalTaxTableV2: List[PTSAdditionalTaxRow] = List(),
                                        additionalTaxTableTotal: String = "0",
                                        reductionsTable: List[(String,String,String)] = List(),
                                        reductionsTableTotal: String = "0",
                                        graph: BandedGraph,
                                        hasChanges: Boolean = false,
                                        ukDividends: Option[TaxComponent],
                                        taxBands: Option[List[TaxBand]],
                                        incomeTaxReducedToZeroMessage: Option[String],
                                        nextYearTaxTotal: BigDecimal =0,
                                        hasPSA: Boolean = false,
                                        hasSSR: Boolean = false,
                                        newGraph:BandedGraph
                                      )

case class PTSAdditionalTaxRow(description:String, amount:String)




case class EstimatedIncomeViewModel(
                                     increasesTax: Boolean = false,
                                     incomeTaxEstimate: BigDecimal = 0,
                                     incomeEstimate: BigDecimal = 0,
                                     taxFreeEstimate: BigDecimal = 0,
                                     taxRelief: Boolean = false,
                                     taxCodes: List[String] = List(),
                                     potentialUnderpayment: Boolean = false,
                                     additionalTaxTable: List[AdditionalTaxRow] = List(),
                                     additionalTaxTableTotal: BigDecimal = BigDecimal(0),
                                     reductionsTable: List[ReductionsRow] = List(),
                                     reductionsTableTotal: BigDecimal = BigDecimal(0),
                                     graph: BandedGraph,
                                     hasChanges: Boolean = false,
                                     ukDividends: Option[TaxComponent],
                                     taxBands: Option[List[TaxBand]],
                                     incomeTaxReducedToZeroMessage: Option[String],
                                     nextYearTaxTotal: BigDecimal =0,
                                     hasPSA: Boolean = false,
                                     hasSSR: Boolean = false
                                   )

case class AdditionalTaxRow(description: String, amount: BigDecimal)
case class ReductionsRow(description: String, amount: BigDecimal, additionalInfo: String)

case class BandedGraph(
                        id:String,
                        bands:List[Band] = List(),
                        minBand :BigDecimal =0,
                        nextBand :BigDecimal = 0,
                        incomeTotal:BigDecimal = 0,
                        zeroIncomeAsPercentage: BigDecimal =0,
                        zeroIncomeTotal: BigDecimal =0,
                        incomeAsPercentage: BigDecimal =0,
                        taxTotal:BigDecimal =0,
                        nextBandMessage: Option[String] = None
                      )

case class Band(
                 colour:String,
                 barPercentage: BigDecimal = 0,
                 tablePercentage: String = "0",
                 income: BigDecimal = 0,
                 tax: BigDecimal = 0,
                 bandType: String
               )

object Band {
  implicit val format = Json.format[Band]
}

object BandedGraph {
  implicit val format = Json.format[BandedGraph]
}

object AdditionalTaxRow {
  implicit val format = Json.format[AdditionalTaxRow]
}

object ReductionsRow {
  implicit val format = Json.format[ReductionsRow]
}

object EstimatedIncomeViewModel {

  implicit val format = Json.format[EstimatedIncomeViewModel]
}

object PTSAdditionalTaxRow {
  implicit val format = Json.format[PTSAdditionalTaxRow]
}

object PTSEstimatedIncomeViewModel {
  import TupleFormats._

  implicit val format = Json.format[PTSEstimatedIncomeViewModel]
}
