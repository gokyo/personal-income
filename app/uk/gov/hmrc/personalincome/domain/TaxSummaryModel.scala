/*
 * Copyright 2016 HM Revenue & Customs
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
import play.api.libs.json._

case class TaxBand(income: Option[BigDecimal]=None, tax: Option[BigDecimal]=None,
                    lowerBand: Option[BigDecimal]=None, upperBand: Option[BigDecimal]=None,
                    rate: Option[BigDecimal]=None)

object TaxBand {
   implicit val formats = Json.format[TaxBand]
 }

case class EditableDetails(isEditable : Boolean = false,
                            payRollingBiks : Boolean = false)

object EditableDetails {
   implicit val formats = Json.format[EditableDetails]
 }

case class IncomeExplanation(employerName : String,
                              inYear : Boolean = false,
                              incomeId : Int = 0,
                              paymentDate : Option[LocalDate] = None,
                              notificationDate : Option[LocalDate] = None,
                              startDate :  Option[LocalDate] = None,
                              endDate :  Option[LocalDate] = None,
                              isCeased : Boolean = false,
                              isPension : Boolean = false,
                              isJSA : Boolean = false,
                              isRtiCalc : Boolean = false,
                              isUpdatedOnline : Boolean = false,
                              isUpdatedTelephone : Boolean = false,
                              isDefaultPrimary : Boolean = false,
                              isDefaultSecondary : Boolean = false,
                              isIrregularPrimary : Boolean = false,
                              isIrregularSecondary : Boolean = false,
                              payToDate : BigDecimal = BigDecimal(0),
                              isMonthly : Boolean = false,
                              amount : BigDecimal = BigDecimal(0),
                              editableDetails : EditableDetails = EditableDetails())

object IncomeExplanation {
   implicit val formats = Json.format[IncomeExplanation]
 }

case class RtiCalc(employmentType : Int,
                    employmentStatus : Int,
                    employerName : String,
                    totalPayToDate : BigDecimal,
                    calculationResult : BigDecimal,
                    startDate : LocalDate,
                    isIrregular : Boolean,
                    inYear : Boolean,
                    isMonthly : Boolean = false)

object RtiCalc {
   implicit val format = Json.format[RtiCalc]
 }


case class RtiData(rtiCalcs : List[RtiCalc])

object RtiData {
   implicit val format = Json.format[RtiData]
 }


case class IncomeData(incomeExplanations: List[IncomeExplanation])

object IncomeData {
   implicit val format = Json.format[IncomeData]
 }

case class TaxCodeComponent(
                              description : Option[String] = None,
                              amount : Option[BigDecimal] = None,
                              componentType: Option[Int]
                              )

object TaxCodeComponent {
   implicit val format = Json.format[TaxCodeComponent]
 }

case class Employments(
                         id: Option[Int] = None,
                         name: Option[String] = None,
                         taxCode: Option[String]
                         )

object Employments {
   implicit val format = Json.format[Employments]
 }

case class TaxCode(taxCode : Option[String],
                    rate : Option[BigDecimal])

object TaxCode {
   implicit val format = Json.format[TaxCode]
 }


case class TaxCodeDescription(taxCode:String,name:String,taxCodeDescriptors: List[TaxCode])

object TaxCodeDescription {
   implicit val format = Json.format[TaxCodeDescription]
 }

case class TaxCodeDetails(employment: Option[List[Employments]],
                           taxCode: Option[List[TaxCode]],
                           taxCodeDescriptions: Option[List[TaxCodeDescription]] = None,
                           deductions: Option[List[TaxCodeComponent]],
                           allowances: Option[List[TaxCodeComponent]],
                           splitAllowances: Option[Boolean] = None,
                           total : BigDecimal = BigDecimal(0))

object TaxCodeDetails {
   implicit val formats = Json.format[TaxCodeDetails]
 }

case class Tax(totalIncome : Option[BigDecimal] = None,
                totalTaxableIncome: Option[BigDecimal] = None,
                totalTax : Option[BigDecimal] = None,
                potentialUnderpayment : Option[BigDecimal] = None,
                taxBands : Option[List[TaxBand]] = None,
                allowReliefDeducts : Option[BigDecimal] = None,
                actualTaxDueAssumingBasicRateAlreadyPaid : Option[BigDecimal] = None,
                actualTaxDueAssumingAllAtBasicRate : Option[BigDecimal] = None)

object Tax {
   implicit val formats = Json.format[Tax]
 }

case class IabdSummary(iabdType : Int, description : String, amount : BigDecimal, employmentId : Option[Int] = None, employmentName : Option[String] = None)

object IabdSummary {
   implicit val formats = Json.format[IabdSummary]
 }

case class TaxComponent(amount : BigDecimal, componentType : Int, description : String, iabdSummaries : List[IabdSummary])

object TaxComponent {
   implicit val formats = Json.format[TaxComponent]
 }

case class TaxCodeIncomeSummary( name : String, taxCode : String,
                                  employmentId : Option[Int] = None,
                                  employmentPayeRef:Option[String] = None,
                                  employmentType : Option[Int] = None,
                                  incomeType: Option[Int] = None,
                                  employmentStatus :Option[Int] = None,
                                  tax : Tax,
                                  worksNumber : Option[String]=None,
                                  jobTitle : Option[String] = None,
                                  startDate : Option[LocalDate] = None,
                                  endDate : Option[LocalDate] = None,
                                  income : Option[BigDecimal]=None,
                                  otherIncomeSourceIndicator:Option[Boolean] = None,
                                  isEditable :Boolean = false,
                                  isLive : Boolean=false,
                                  isOccupationalPension: Boolean=false,
                                  isPrimary: Boolean=true)

object TaxCodeIncomeSummary {
   implicit val formats = Json.format[TaxCodeIncomeSummary]
 }

case class TaxCodeIncomeTotal(taxCodeIncomes: List[TaxCodeIncomeSummary],
                               totalIncome : BigDecimal,
                               totalTax: BigDecimal,
                               totalTaxableIncome : BigDecimal )

object TaxCodeIncomeTotal {
   implicit val formats = Json.format[TaxCodeIncomeTotal]
 }

case class NoneTaxCodeIncomes(statePension: Option[BigDecimal] = None,
                               statePensionLumpSum: Option[BigDecimal] = None,
                               otherPensions: Option[TaxComponent] = None,
                               otherIncome : Option[TaxComponent] = None,
                               taxableStateBenefit: Option[TaxComponent] = None,
                               untaxedInterest : Option[TaxComponent] = None,
                               bankBsInterest : Option[TaxComponent] = None,
                               dividends : Option[TaxComponent] = None,
                               foreignInterest : Option[TaxComponent] = None,
                               foreignDividends : Option[TaxComponent] = None,
                               totalIncome : BigDecimal) {
                                }

object NoneTaxCodeIncomes {
   implicit val formats = Json.format[NoneTaxCodeIncomes]
 }

case class TaxCodeIncomes(employments: Option[TaxCodeIncomeTotal] = None,
                           occupationalPensions: Option[TaxCodeIncomeTotal] = None,
                           taxableStateBenefitIncomes: Option[TaxCodeIncomeTotal] = None,
                           ceasedEmployments: Option[TaxCodeIncomeTotal] = None,
                           hasDuplicateEmploymentNames: Boolean,
                           totalIncome : BigDecimal,
                           totalTaxableIncome : BigDecimal,
                           totalTax : BigDecimal)

object TaxCodeIncomes {
   implicit val formats = Json.format[TaxCodeIncomes]
 }

case class Incomes(taxCodeIncomes : TaxCodeIncomes, noneTaxCodeIncomes : NoneTaxCodeIncomes, total: BigDecimal)

object Incomes {
   implicit val formats = Json.format[Incomes]
 }

case class IncreasesTax(incomes : Option[Incomes] = None,
                         benefitsFromEmployment : Option[TaxComponent] = None,
                         total : BigDecimal)

object IncreasesTax {
   implicit val formats = Json.format[IncreasesTax]
 }

case class DecreasesTax(personalAllowance : Option[BigDecimal] = None,
                         personalAllowanceSourceAmount: Option[BigDecimal] = None,
                         blindPerson : Option[TaxComponent] = None,
                         expenses : Option[TaxComponent] = None,
                         giftRelated : Option[TaxComponent] = None,
                         jobExpenses : Option[TaxComponent] = None,
                         miscellaneous : Option[TaxComponent] = None,
                         pensionContributions : Option[TaxComponent] = None,
                         paTransferredAmount : Option[BigDecimal] = None,
                         paReceivedAmount : Option[BigDecimal] = None,
                         paTapered : Boolean = false,
                         personalSavingsAllowance : Option[TaxComponent] = None,
                         total : BigDecimal)

object DecreasesTax {
   implicit val formats = Json.format[DecreasesTax]
 }

case class ExtensionRelief(sourceAmount : BigDecimal = BigDecimal(0),
                            reliefAmount : BigDecimal = BigDecimal(0))


object ExtensionRelief {
   implicit val formats = Json.format[ExtensionRelief]
 }

case class ExtensionReliefs(giftAid : Option[ExtensionRelief] = None,
                             personalPension :Option[ExtensionRelief] = None)

object ExtensionReliefs {
   implicit val formats = Json.format[ExtensionReliefs]
 }

case class MarriageAllowance(marriageAllowance:BigDecimal = BigDecimal(0), marriageAllowanceRelief:BigDecimal = BigDecimal(0))

object MarriageAllowance {
   implicit val formats = Json.format[MarriageAllowance]
 }

case class Adjustment(codingAmount:BigDecimal = BigDecimal(0), amountInTermsOfTax:BigDecimal = BigDecimal(0))

object Adjustment {
   implicit val formats = Json.format[Adjustment]
 }

case class LiabilityReductions(marriageAllowance: Option[MarriageAllowance] = None,
                                enterpriseInvestmentSchemeRelief : Option[Adjustment]=None,
                                concessionalRelief: Option[Adjustment]=None,
                                maintenancePayments: Option[Adjustment]=None,
                                doubleTaxationRelief: Option[Adjustment]=None)

object LiabilityReductions {
   implicit val formats = Json.format[LiabilityReductions]
 }

case class LiabilityAdditions( excessGiftAidTax : Option[Adjustment]=None,
                                excessWidowsAndOrphans: Option[Adjustment]=None,
                                pensionPaymentsAdjustment: Option[Adjustment]=None)

object LiabilityAdditions {
   implicit val formats = Json.format[LiabilityAdditions]
 }


case class TotalLiability(nonSavings:Option[Tax]=None,
                           nonCodedIncome:Option[Tax]=None,
                           untaxedInterest : Option[Tax] = None,
                           bankInterest : Option[Tax] = None,
                           ukDividends : Option[Tax] = None,
                           foreignInterest : Option[Tax] = None,
                           foreignDividends : Option[Tax] = None,
                           mergedIncomes : Option[Tax] = None,
                           totalLiability : Option[BigDecimal] = None,
                           totalTax:BigDecimal,
                           totalTaxOnIncome : BigDecimal = BigDecimal(0),
                           underpaymentPreviousYear: BigDecimal = BigDecimal(0),
                           outstandingDebt: BigDecimal = BigDecimal(0),
                           childBenefitAmount: BigDecimal = BigDecimal(0),
                           childBenefitTaxDue: BigDecimal = BigDecimal(0),
                           taxOnBankBSInterest : Option[BigDecimal] = None,
                           taxCreditOnUKDividends : Option[BigDecimal] = None,
                           taxCreditOnForeignInterest : Option[BigDecimal] = None,
                           taxCreditOnForeignIncomeDividends : Option[BigDecimal] = None,
                           liabilityReductions : Option[LiabilityReductions] = None,
                           liabilityAdditions : Option[LiabilityAdditions] = None)

object TotalLiability {
   implicit val formats = Json.format[TotalLiability]
 }


case class GateKeeperRule(gateKeeperType : Int, id : Int, description : String)

object GateKeeperRule {
   implicit val formats = Json.format[GateKeeperRule]
 }


case class GateKeeper(gateKeepered:Boolean=false,
                       gateKeeperResults : List[GateKeeperRule])

object GateKeeper {
   implicit val formats = Json.format[GateKeeper]
 }

case class Change[A,B](currentYear:A,currentYearPlusOne:B)

object Change {
   implicit val emp = Employments.format

   implicit def changeReads[A, B]  (implicit aReads: Reads[A], bReads: Reads[B]) : Reads[Change[A,B]] = new Reads[Change[A, B]] {
     override def reads(json: JsValue): JsResult[Change[A,B]] = {
       json match {
         case JsObject (js) =>
           val a = aReads.reads(json \ "currentYear").get
           val b = bReads.reads(json \ "currentYearPlusOne").get
           JsSuccess(Change(a,b))

         case _ => JsError("Failed to read non JsObject!")
       }

     }
   }

   implicit def changeWrite[A, B]  (implicit aWrites: Writes[A], bWrites: Writes[B]) =  new Writes[Change[A,B]] {
     def writes(change: Change[A,B]) = {
       val currentYearJs = aWrites.writes(change.currentYear)
       val currentYearPlusOneJs = bWrites.writes(change.currentYearPlusOne)
       JsObject(Seq(
         "currentYear" -> currentYearJs,
         "currentYearPlusOne" -> currentYearPlusOneJs))
     }
   }

 }

case class CYPlusOneChange(
                             employments: Option[List[Change[Option[Employments],Option[Employments]]]] = None,
                             scottishEmployments: Option[List[Change[Option[Employments],Option[Employments]]]] = None,
                             personalAllowance: Option[Change[BigDecimal,BigDecimal]] = None,
                             underPayment: Option[Change[BigDecimal,BigDecimal]] = None,
                             totalTax: Option[Change[BigDecimal,BigDecimal]] = None,
                             standardPA: Option[BigDecimal] = None,
                             employmentBenefits: Option[Boolean] = None,
                             dividendsTax :Option[Change[BigDecimal,BigDecimal]] = None,
                             personalSavingsAllowance :Option[Change[BigDecimal,BigDecimal]] = None
                             )

object CYPlusOneChange {
   implicit val formats = Json.format[CYPlusOneChange]
 }

case class TaxSummaryDetails( nino: String,
                               version: Int,
                               increasesTax: Option[IncreasesTax] = None,
                               decreasesTax: Option[DecreasesTax] = None,
                               other : Option[TaxComponent] = None,
                               totalLiability : Option[TotalLiability] = None,
                               workingTaxableTotal : BigDecimal = BigDecimal(0),
                               actualTaxableTotal : BigDecimal = BigDecimal(0),
                               remainingTaxFree : BigDecimal = BigDecimal(0),
                               adjustedNetIncome: BigDecimal = BigDecimal(0),
                               extensionReliefs: Option[ExtensionReliefs] = None,
                               incomeGateKeeper : Option[GateKeeper]=None,
                               gateKeeper:Option[GateKeeper]=None,
                               taxCodeDetails : Option[TaxCodeDetails] = None,
                               incomeData : Option[IncomeData] = None,
                               cyPlusOneChange:Option[CYPlusOneChange] = None,
                               cyPlusOneSummary:Option[TaxSummaryDetails] = None)

object TaxSummaryDetails {
   implicit val formats = Json.format[TaxSummaryDetails]
 }
