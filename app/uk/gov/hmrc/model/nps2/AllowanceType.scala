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

package uk.gov.hmrc.model.nps2

object AllowanceType extends Enumeration {
  val JobExpenses = Value(1)
  val FlatRateJobExpenses = Value(2)
  val ProfessionalSubscriptions = Value(3)
  val PaymentsTowardsARetirementAnnuity = Value(4)
  val PersonalPensionRelief = Value(5)
  val GiftAidPayments = Value(6)
  val EnterpriseInvestmentSchemeRelief = Value(7)
  val LoanInterest = Value(8)
  val LossRelief = Value(9)
  val MaintenancePayments = Value(10)
  val PersonalAllowanceStandard = Value(11)
  val PersonalAllowanceAged = Value(12)
  val PersonalAllowanceElderly = Value(13)
  val MarriedCouplesAllowance = Value(15)
  val MarriedCouplesAllowance2 = Value(16)
  val MarriedCouplesAllowance3 = Value(17)
  val MarriedCouplesAllowance4 = Value(18)
  val MarriedCouplesAllowanceFromHusband = Value(19)
  val MarriedCouplesAllowanceFromHusband2 = Value(20)
  val MarriedCouplesAllowance5 = Value(21)
  val BlindPersonsAllowance = Value(22)
  val BalanceOfTaxAllowances = Value(23)
  val DeathSicknessOrFuneralBenefits = Value(24)
  val DeathSicknessOrFuneralBenefits2 = Value(25)
  val DeathSicknessOrFuneralBenefits3 = Value(26)
  val StartingRateAdjustment = Value(27)
  val ConcessionalRelief = Value(28)
  val DoubleTaxationRelief = Value(29)
  val ForeignPensionAllowance = Value(30)
  val EarlierYearsAdjustment = Value(31)
}
