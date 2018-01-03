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

import org.joda.time.LocalDate
import play.api.libs.json.Format
import play.api.libs.json._
import uk.gov.hmrc.model.tai.{TaxYear, AnnualAccount}
import uk.gov.hmrc.model.rti._
import play.api.libs.functional.syntax._

case class TaxBand(income: Option[BigDecimal] = None, tax: Option[BigDecimal] = None,
                   lowerBand: Option[BigDecimal] = None, upperBand: Option[BigDecimal] = None,
                   rate: Option[BigDecimal] = None)

object TaxBand {
  implicit val formats: Format[TaxBand] = Json.format[TaxBand]
}

case class EditableDetails(isEditable: Boolean = false,
                           payRollingBiks: Boolean = false)

object EditableDetails {
  implicit val formats: Format[EditableDetails] = Json.format[EditableDetails]
}

case class IncomeExplanation(employerName : String,
                             incomeId : Int = 0,
                             hasDuplicateEmploymentNames: Boolean = false,
                             worksNumber : Option[String] = None,
                             paymentDate : Option[LocalDate] = None,
                             notificationDate : Option[LocalDate] = None,
                             updateActionDate : Option[LocalDate] = None,
                             startDate :  Option[LocalDate] = None,
                             endDate :  Option[LocalDate] = None,
                             employmentStatus : Option[Int] = None,
                             employmentType: Option[Int] = None,
                             isPension : Boolean = false,
                             iabdSource : Option[Int] = None,
                             payToDate : BigDecimal = BigDecimal(0),
                             calcAmount : Option[BigDecimal] = None,
                             grossAmount : Option[BigDecimal] = None,
                             payFrequency : Option[PayFrequency.Value] = None,
                             cessationPay: Option[BigDecimal] = None,
                             editableDetails : EditableDetails = EditableDetails())

object IncomeExplanation {
  implicit val formats: Format[IncomeExplanation] = Json.format[IncomeExplanation]
}

case class CeasedEmploymentDetails(endDate: Option[LocalDate], isPension: Option[Boolean], ceasedStatus: Option[String])

object CeasedEmploymentDetails {
  implicit val formats = Json.format[CeasedEmploymentDetails]
}

case class RtiCalc(employmentType: Int,
                   employmentStatus: Int,
                   employerName: String,
                   totalPayToDate: BigDecimal,
                   calculationResult: Option[BigDecimal],
                   startDate: LocalDate,
                   isIrregular: Boolean,
                   inYear: Boolean,
                   isMonthly: Boolean = false)

object RtiCalc {
  implicit val format: Format[RtiCalc] = Json.format[RtiCalc]
}


case class RtiData(rtiCalcs: List[RtiCalc])

object RtiData {
  implicit val format: Format[RtiData] = Json.format[RtiData]
}


case class IncomeData(incomeExplanations: List[IncomeExplanation])

object IncomeData {
  implicit val format: Format[IncomeData] = Json.format[IncomeData]
}

case class TaxCodeComponent(
                             description: Option[String] = None,
                             amount: Option[BigDecimal] = None,
                             componentType: Option[Int]
                           )

object TaxCodeComponent {
  implicit val format: Format[TaxCodeComponent] = Json.format[TaxCodeComponent]
}

case class Employments(
                        id: Option[Int] = None,
                        name: Option[String] = None,
                        taxCode: Option[String]
                      )

object Employments {
  implicit val format: Format[Employments] = Json.format[Employments]
}

case class TaxCode(taxCode: Option[String],
                   rate: Option[BigDecimal])

object TaxCode {
  implicit val format: Format[TaxCode] = Json.format[TaxCode]
}


case class TaxCodeDescription(taxCode: String, name: String, taxCodeDescriptors: List[TaxCode])

object TaxCodeDescription {
  implicit val format: Format[TaxCodeDescription] = Json.format[TaxCodeDescription]
}

case class TaxCodeDetails(employment: Option[List[Employments]],
                          taxCode: Option[List[TaxCode]],
                          taxCodeDescriptions: Option[List[TaxCodeDescription]] = None,
                          deductions: Option[List[TaxCodeComponent]],
                          allowances: Option[List[TaxCodeComponent]],
                          splitAllowances: Option[Boolean] = None,
                          total: BigDecimal = BigDecimal(0))

object TaxCodeDetails {
  implicit val formats: Format[TaxCodeDetails] = Json.format[TaxCodeDetails]
}

case class Tax(totalIncome: Option[BigDecimal] = None,
               totalTaxableIncome: Option[BigDecimal] = None,
               totalTax: Option[BigDecimal] = None,
               totalInYearAdjustment: Option[BigDecimal] = None,
               inYearAdjustmentIntoCY:Option[BigDecimal] = None,
               inYearAdjustmentIntoCYPlusOne:Option[BigDecimal] = None,
               inYearAdjustmentFromPreviousYear:Option[BigDecimal] = None,
               taxBands: Option[List[TaxBand]] = None,
               allowReliefDeducts: Option[BigDecimal] = None,
               actualTaxDueAssumingBasicRateAlreadyPaid: Option[BigDecimal] = None,
               actualTaxDueAssumingAllAtBasicRate: Option[BigDecimal] = None)

object Tax {
  implicit val formats: Format[Tax] = Json.format[Tax]
}

case class IabdSummary(iabdType: Int, description: String, amount: BigDecimal, employmentId: Option[Int] = None, employmentName: Option[String] = None)

object IabdSummary {
  implicit val formats: Format[IabdSummary] = Json.format[IabdSummary]
}

case class TaxComponent(amount: BigDecimal, componentType: Int, description: String, iabdSummaries: List[IabdSummary])

object TaxComponent {
  implicit val formats: Format[TaxComponent] = Json.format[TaxComponent]
}

case class TaxCodeIncomeSummary(name: String, taxCode: String,
                                employmentId: Option[Int] = None,
                                employmentPayeRef: Option[String] = None,
                                employmentType: Option[Int] = None,
                                incomeType: Option[Int] = None,
                                employmentStatus: Option[Int] = None,
                                tax: Tax,
                                worksNumber: Option[String] = None,
                                jobTitle: Option[String] = None,
                                startDate: Option[LocalDate] = None,
                                endDate: Option[LocalDate] = None,
                                income: Option[BigDecimal] = None,
                                otherIncomeSourceIndicator: Option[Boolean] = None,
                                isEditable: Boolean = false,
                                isLive: Boolean = false,
                                isOccupationalPension: Boolean = false,
                                isPrimary: Boolean = true)

object TaxCodeIncomeSummary {
  implicit val formats: Format[TaxCodeIncomeSummary] = Json.format[TaxCodeIncomeSummary]
}

case class TaxCodeIncomeTotal(taxCodeIncomes: List[TaxCodeIncomeSummary],
                              totalIncome: BigDecimal,
                              totalTax: BigDecimal,
                              totalTaxableIncome: BigDecimal)

object TaxCodeIncomeTotal {
  implicit val formats: Format[TaxCodeIncomeTotal] = Json.format[TaxCodeIncomeTotal]
}

case class NoneTaxCodeIncomes(statePension: Option[BigDecimal] = None,
                              statePensionLumpSum: Option[BigDecimal] = None,
                              otherPensions: Option[TaxComponent] = None,
                              otherIncome: Option[TaxComponent] = None,
                              taxableStateBenefit: Option[TaxComponent] = None,
                              untaxedInterest: Option[TaxComponent] = None,
                              bankBsInterest: Option[TaxComponent] = None,
                              dividends: Option[TaxComponent] = None,
                              foreignInterest: Option[TaxComponent] = None,
                              foreignDividends: Option[TaxComponent] = None,
                              totalIncome: BigDecimal) {
}

object NoneTaxCodeIncomes {
  implicit val formats: Format[NoneTaxCodeIncomes] = Json.format[NoneTaxCodeIncomes]
}

case class TaxCodeIncomes(employments: Option[TaxCodeIncomeTotal] = None,
                          occupationalPensions: Option[TaxCodeIncomeTotal] = None,
                          taxableStateBenefitIncomes: Option[TaxCodeIncomeTotal] = None,
                          ceasedEmployments: Option[TaxCodeIncomeTotal] = None,
                          hasDuplicateEmploymentNames: Boolean,
                          totalIncome: BigDecimal,
                          totalTaxableIncome: BigDecimal,
                          totalTax: BigDecimal)

object TaxCodeIncomes {
  implicit val formats: Format[TaxCodeIncomes] = Json.format[TaxCodeIncomes]
}

case class Incomes(taxCodeIncomes: TaxCodeIncomes, noneTaxCodeIncomes: NoneTaxCodeIncomes, total: BigDecimal)

object Incomes {
  implicit val formats: Format[Incomes] = Json.format[Incomes]
}

case class IncreasesTax(incomes: Option[Incomes] = None,
                        benefitsFromEmployment: Option[TaxComponent] = None,
                        total: BigDecimal)

object IncreasesTax {
  implicit val formats: Format[IncreasesTax] = Json.format[IncreasesTax]
}

case class DecreasesTax(personalAllowance: Option[BigDecimal] = None,
                        personalAllowanceSourceAmount: Option[BigDecimal] = None,
                        blindPerson: Option[TaxComponent] = None,
                        expenses: Option[TaxComponent] = None,
                        giftRelated: Option[TaxComponent] = None,
                        jobExpenses: Option[TaxComponent] = None,
                        miscellaneous: Option[TaxComponent] = None,
                        pensionContributions: Option[TaxComponent] = None,
                        paTransferredAmount: Option[BigDecimal] = None,
                        paReceivedAmount: Option[BigDecimal] = None,
                        paTapered: Boolean = false,
                        personalSavingsAllowance: Option[TaxComponent] = None,
                        total: BigDecimal)

object DecreasesTax {
  implicit val formats: Format[DecreasesTax] = Json.format[DecreasesTax]
}

case class ExtensionRelief(sourceAmount: BigDecimal = BigDecimal(0),
                           reliefAmount: BigDecimal = BigDecimal(0))


object ExtensionRelief {
  implicit val formats: Format[ExtensionRelief] = Json.format[ExtensionRelief]
}

case class ExtensionReliefs(giftAid: Option[ExtensionRelief] = None,
                            personalPension: Option[ExtensionRelief] = None)

object ExtensionReliefs {
  implicit val formats: Format[ExtensionReliefs] = Json.format[ExtensionReliefs]
}

case class MarriageAllowance(marriageAllowance: BigDecimal = BigDecimal(0), marriageAllowanceRelief: BigDecimal = BigDecimal(0))

object MarriageAllowance {
  implicit val formats: Format[MarriageAllowance] = Json.format[MarriageAllowance]
}

case class Adjustment(codingAmount: BigDecimal = BigDecimal(0), amountInTermsOfTax: BigDecimal = BigDecimal(0))

object Adjustment {
  implicit val formats: Format[Adjustment] = Json.format[Adjustment]
}

case class LiabilityReductions(marriageAllowance: Option[MarriageAllowance] = None,
                               enterpriseInvestmentSchemeRelief: Option[Adjustment] = None,
                               concessionalRelief: Option[Adjustment] = None,
                               maintenancePayments: Option[Adjustment] = None,
                               doubleTaxationRelief: Option[Adjustment] = None)

object LiabilityReductions {
  implicit val formats: Format[LiabilityReductions] = Json.format[LiabilityReductions]
}

case class LiabilityAdditions(excessGiftAidTax: Option[Adjustment] = None,
                              excessWidowsAndOrphans: Option[Adjustment] = None,
                              pensionPaymentsAdjustment: Option[Adjustment] = None)

object LiabilityAdditions {
  implicit val formats: Format[LiabilityAdditions] = Json.format[LiabilityAdditions]
}


case class TotalLiability(
                           nonCodedIncome: Option[Tax] = None,
                           totalTax: BigDecimal,
                           underpaymentPreviousYear: BigDecimal = BigDecimal(0),
                           inYearAdjustment: Option[BigDecimal] = None,
                           outstandingDebt: BigDecimal = BigDecimal(0),
                           childBenefitTaxDue: BigDecimal = BigDecimal(0),
                           taxOnBankBSInterest: Option[BigDecimal] = None,
                           taxCreditOnUKDividends: Option[BigDecimal] = None,
                           taxCreditOnForeignInterest: Option[BigDecimal] = None,
                           taxCreditOnForeignIncomeDividends: Option[BigDecimal] = None,
                           liabilityReductions: Option[LiabilityReductions] = None,
                           liabilityAdditions: Option[LiabilityAdditions] = None)

object TotalLiability {
  implicit val formats: Format[TotalLiability] = Json.format[TotalLiability]
}


case class GateKeeperRule(gateKeeperType: Int, id: Int, description: String)

object GateKeeperRule {
  implicit val formats: Format[GateKeeperRule] = Json.format[GateKeeperRule]
}


case class GateKeeper(gateKeepered: Boolean = false,
                      gateKeeperResults: List[GateKeeperRule])

object GateKeeper {
  implicit val formats: Format[GateKeeper] = Json.format[GateKeeper]
}

case class Change[A, B](currentYear: A, currentYearPlusOne: B)

object Change {
  implicit val emp = Employments.format
  implicit val changeDecimal = Json.format[Change[BigDecimal, BigDecimal]]
  implicit val changeEmployments = Json.format[Change[Option[Employments], Option[Employments]]]
}

case class CYPlusOneChange(
                            employmentsTaxCode: Option[List[Employments]] = None,
                            scottishTaxCodes: Option[Boolean] = None,
                            personalAllowance: Option[Change[BigDecimal,BigDecimal]] = None,
                            underPayment: Option[Change[BigDecimal,BigDecimal]] = None,
                            totalTax: Option[Change[BigDecimal,BigDecimal]] = None,
                            standardPA: Option[BigDecimal] = None,
                            employmentBenefits: Option[Boolean] = None,
                            personalSavingsAllowance :Option[Change[BigDecimal,BigDecimal]] = None
                          )

object CYPlusOneChange {
  implicit val formats: Format[CYPlusOneChange] = Json.format[CYPlusOneChange]
}


case class TaxSummaryDetails(nino: String,
                             version: Int,
                             increasesTax: Option[IncreasesTax] = None,
                             decreasesTax: Option[DecreasesTax] = None,
                             totalLiability: Option[TotalLiability] = None,
                             adjustedNetIncome: BigDecimal = BigDecimal(0),
                             extensionReliefs: Option[ExtensionReliefs] = None,
                             gateKeeper: Option[GateKeeper] = None,
                             taxCodeDetails: Option[TaxCodeDetails] = None,
                             incomeData: Option[IncomeData] = None,
                             cyPlusOneChange: Option[CYPlusOneChange] = None,
                             cyPlusOneSummary: Option[TaxSummaryDetails] = None,
                             accounts: Seq[AnnualAccount] = Nil,
                             ceasedEmploymentDetail: Option[CeasedEmploymentDetails] = None
                            ) {
  def currentYearAccounts: Option[AnnualAccount] = accounts.find { annualAccounts =>
    annualAccounts.year == TaxYear()
  }

  def previousYearAccount: Option[AnnualAccount] = accounts.find { annualAccounts =>
    annualAccounts.year == TaxYear().prev
  }
}

object TaxSummaryDetails {
  implicit val formats = Json.format[TaxSummaryDetails]
}

case class TaxSummaryDetailsResponse( nino: String,
                              version: Int,
                              increasesTax: Option[IncreasesTax] = None,
                              decreasesTax: Option[DecreasesTax] = None,
                              totalLiability : Option[TotalLiability] = None,
                              extensionReliefs: Option[ExtensionReliefs] = None,
                              gateKeeper:Option[GateKeeper]=None,
                              taxCodeDetails : Option[TaxCodeDetails] = None,
                              cyPlusOneChange:Option[CYPlusOneChange] = None)

object TaxSummaryDetailsResponse {

  implicit val reads: Reads[TaxSummaryDetailsResponse] =
    (
      (JsPath \ "nino").read[String] and
        (JsPath \ "version").read[Int] and
        (JsPath \ "increasesTax").readNullable[IncreasesTax] and
        (JsPath \ "decreasesTax").readNullable[DecreasesTax] and
        (JsPath \ "totalLiability").readNullable[TotalLiability] and
        (JsPath \ "extensionReliefs").readNullable[ExtensionReliefs] and
        (JsPath \ "gateKeeper").readNullable[GateKeeper] and
        (JsPath \ "taxCodeDetails").readNullable[TaxCodeDetails] and
        (JsPath \ "cyPlusOneChange").readNullable[CYPlusOneChange]
      )(TaxSummaryDetailsResponse.apply _)

  implicit val writes: Writes[TaxSummaryDetailsResponse] =
    (
      (JsPath \ "nino").write[String] and
        (JsPath \ "version").write[Int] and
        (JsPath \ "increasesTax").writeNullable[IncreasesTax] and
        (JsPath \ "decreasesTax").writeNullable[DecreasesTax] and
        (JsPath \ "totalLiability").writeNullable[TotalLiability] and
        (JsPath \ "extensionReliefs").writeNullable[ExtensionReliefs] and
        (JsPath \ "gateKeeper").writeNullable[GateKeeper] and
        (JsPath \ "taxCodeDetails").writeNullable[TaxCodeDetails] and
        (JsPath \ "cyPlusOneChange").writeNullable[CYPlusOneChange]
      )(unlift(TaxSummaryDetailsResponse.unapply))

}