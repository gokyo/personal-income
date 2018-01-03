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

package uk.gov.hmrc.personalincome.domain

import play.api.libs.json._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.domain._


trait ViewModelFactory {
  type ViewModelType
  def createObject(nino: Nino, details: TaxSummaryDetails): ViewModelType
}

case class BaseViewModel (estimatedIncomeTax:BigDecimal = 0,
                           taxableIncome:BigDecimal = 0,
                           taxFree:BigDecimal = 0,
                           personalAllowance: BigDecimal = 0,
                           hasTamc: Boolean = false,
                           taxCodesList:List[String] = List(),
                           hasChanges:Boolean = false
                           )

object BaseViewModel {
  implicit val format = Json.format[BaseViewModel]
}

case class Band (colour:String,
                  barPercentage: BigDecimal = 0,
                  tablePercentage: String = "0",
                  income: BigDecimal = 0,
                  tax: BigDecimal = 0
                  )
object Band {
  implicit val format = Json.format[Band]
}

case class BandedGraph (id:String,
                         bands:List[Band] = List(),
                         minBand :BigDecimal =0,
                         nextBand :BigDecimal = 0,
                         incomeTotal:BigDecimal = 0,
                         incomeAsPercentage: BigDecimal =0,
                         taxTotal:BigDecimal =0
                         )
object BandedGraph {
  implicit val format = Json.format[BandedGraph]
}

case class MessageWrapper(a:String, b:String, c:Option[String]=None)
object MessageWrapper {
  implicit val format = Json.format[MessageWrapper]

  def applyForList(a:List[(String, String, String)]) : List[MessageWrapper] = {
    a.map(item => MessageWrapper(item._1, item._2, Some(item._3)))
  }
}

case class EstimatedIncome (increasesTax: Boolean = false,
                             incomeTaxEstimate: BigDecimal = 0,
                             incomeEstimate: BigDecimal = 0,
                             taxFreeEstimate: BigDecimal = 0,
                             taxRelief: Boolean = false,
                             taxCodes: Seq[String] = List(),
                             potentialUnderpayment:Boolean = false,
                             additionalTaxTable :Seq[MessageWrapper] = List(),
                             additionalTaxTableTotal: String = "",
                             reductionsTable :Seq[MessageWrapper] = List(),
                             reductionsTableTotal:String = "",
                             graph: BandedGraph,
                             hasChanges:Boolean = false
                             )

object EstimatedIncome {
  implicit val format = Json.format[EstimatedIncome]
}

case class EstimatedIncomeWrapper(estimatedIncome:EstimatedIncome, potentialUnderpayment:Option[BigDecimal])
object EstimatedIncomeWrapper {
  implicit val format = Json.format[EstimatedIncomeWrapper]
}

case class BenefitsDataWrapper(a:String, b:String, c:String, d:String, e:Option[Int], f:Option[Int])

object BenefitsDataWrapper {
  implicit val format = Json.format[BenefitsDataWrapper]

  def applyBenefit(a:List[(String, String, String, String, Option[Int], Option[Int])]) : List[BenefitsDataWrapper] = {
    a.map(item => BenefitsDataWrapper(item._1, item._2, item._3, item._4, item._5, item._6))
  }
}

case class EmploymentPension (taxCodeIncomes: Option[TaxCodeIncomes],
                               totalEmploymentPensionAmt: BigDecimal = BigDecimal(0),
                               hasEmployment: Boolean = false,
                               isOccupationalPension: Boolean = false
                               )
object EmploymentPension {
  implicit val format = Json.format[EmploymentPension]
}

case class TaxableIncome (taxFreeAmount: BigDecimal,
                           incomeTax:BigDecimal,
                           income: BigDecimal,
                           taxCodeList: List[String],
                           increasesTax: Option[IncreasesTax] = None,
                           employmentPension : EmploymentPension,
                           investmentIncomeData :List[MessageWrapper] = List(),
                           investmentIncomeTotal : BigDecimal,
                           otherIncomeData: List[MessageWrapper] = List(),
                           otherIncomeTotal: BigDecimal,
                           benefitsData :List[BenefitsDataWrapper] = List(),
                           benefitsTotal: BigDecimal,
                           taxableBenefitsData: List[MessageWrapper] = List(),
                           taxableBenefitsTotal: BigDecimal,
                           hasChanges:Boolean = false
                           )
object TaxableIncome {
  implicit val format = Json.format[TaxableIncome]
}


case class GateKeeperDetails (totalLiability: TotalLiability,
                               decreasesTax: DecreasesTax,
                               employmentList: List[MessageWrapper] = List(),
                               increasesTax: IncreasesTax
                               )

object GateKeeperDetails {
  implicit val format = Json.format[GateKeeperDetails]
}
