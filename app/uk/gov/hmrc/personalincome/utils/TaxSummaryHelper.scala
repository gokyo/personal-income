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

package uk.gov.hmrc.personalincome.utils

import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.play.views.helpers.MoneyPounds



object TaxSummaryHelper {


  def sortedTaxableIncomes(incomes: TaxCodeIncomes) = {
    val taxableIncomes = incomes.employments.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.occupationalPensions.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.taxableStateBenefitIncomes.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.ceasedEmployments.map(_.taxCodeIncomes).getOrElse(Nil)

    taxableIncomes.sortBy(employment => (employment.employmentType.getOrElse(0) * 1000) + employment.employmentId.getOrElse(0))
  }

  def getActualPA(decreasesTax: Option[DecreasesTax]): BigDecimal = {
    decreasesTax.map(_.personalAllowance.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0))
  }

  def getPPR(taxSummaryDetails: TaxSummaryDetails): (BigDecimal, BigDecimal) = {

    val pprSource = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.personalPension.map(_.sourceAmount)
    ).getOrElse(BigDecimal(0))

    val pprRelief = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.personalPension.map(_.reliefAmount)
    ).getOrElse(BigDecimal(0))

    (pprSource, pprRelief)
  }

  def getGiftAid(taxSummaryDetails: TaxSummaryDetails): (BigDecimal, BigDecimal) = {

    val giftAidSource = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.giftAid.map(_.sourceAmount)
    ).getOrElse(BigDecimal(0))

    val giftAidRelief = taxSummaryDetails.extensionReliefs.flatMap(extensionRelief =>
      extensionRelief.giftAid.map(_.reliefAmount)
    ).getOrElse(BigDecimal(0))

    (giftAidSource, giftAidRelief)
  }

  def getTotalAdditionalTaxDue(totalLiability: TotalLiability): BigDecimal = {
    totalLiability.underpaymentPreviousYear +
      totalLiability.childBenefitTaxDue +
      totalLiability.outstandingDebt +
      totalLiability.liabilityAdditions.flatMap(_.excessGiftAidTax.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityAdditions.flatMap(_.excessWidowsAndOrphans.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityAdditions.flatMap(_.pensionPaymentsAdjustment.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
  }

  def getTotalReductions(totalLiability: TotalLiability) = {

    totalLiability.taxOnBankBSInterest.getOrElse(BigDecimal(0)) +
      totalLiability.taxCreditOnUKDividends.getOrElse(BigDecimal(0)) +
      totalLiability.taxCreditOnForeignInterest.getOrElse(BigDecimal(0)) +
      totalLiability.taxCreditOnForeignIncomeDividends.getOrElse(BigDecimal(0)) +
      totalLiability.nonCodedIncome.map(_.totalTax.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.marriageAllowance.map(_.marriageAllowanceRelief)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.enterpriseInvestmentSchemeRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.concessionalRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0)) +
      totalLiability.liabilityReductions.flatMap(_.doubleTaxationRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
  }

  def removeDecimalsToString(decimal: math.BigDecimal): String = {
    decimal.bigDecimal.stripTrailingZeros.toPlainString
  }

  def createInvestmentIncomeTable(nonTaxCodeIncomes: Option[NoneTaxCodeIncomes]): (List[(String, String, String)], BigDecimal) = {

    val dividends = {
      val ukDividends = nonTaxCodeIncomes.map(_.dividends).flatten
      if (ukDividends.isDefined)
        ukDividends.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())
    }

    val bankInterest = {
      val interest = nonTaxCodeIncomes.map(_.bankBsInterest).flatten
      if (interest.isDefined)
        interest.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())
    }

    val unTaxedBankInterest = {
      val unTaxedInterest = nonTaxCodeIncomes.map(_.untaxedInterest).flatten
      if (unTaxedInterest.isDefined)
        unTaxedInterest.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())
    }

    val foreignInterest = {
      val foreigninterest = nonTaxCodeIncomes.map(_.foreignInterest).flatten
      if (foreigninterest.isDefined)
        foreigninterest.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())
    }

    val foreignDividends = {
      val foreigndividends = nonTaxCodeIncomes.map(_.foreignDividends).flatten
      if (foreigndividends.isDefined)
        foreigndividends.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())
    }


    val totalInvestmentIncome: BigDecimal = dividends.amount +
      bankInterest.amount +
      unTaxedBankInterest.amount +
      foreignInterest.amount +
      foreignDividends.amount

    val dividendsRows: List[Option[(String, String, String)]] =
      for {
        iabdSummary <- dividends.iabdSummaries

        message: (String, String) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
              Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"))
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "")
          }

        }

        rawAmt = iabdSummary.amount
        amount = MoneyPounds(rawAmt, 0).quantity

        dividendsData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (dividendsData)

    val bankInterestRows: List[Option[(String, String, String)]] =
      for {
        iabdSummary <- bankInterest.iabdSummaries

        message: (String, String) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
              Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"))
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "")
          }
        }

        rawAmt = iabdSummary.amount
        amount = MoneyPounds(rawAmt, 0).quantity

        bankInterestData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (bankInterestData)

    val unTaxedInterestRows: List[Option[(String, String, String)]] =
      for {
        iabdSummary <- unTaxedBankInterest.iabdSummaries

        message: (String, String) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
              Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"))
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "")
          }
        }
        rawAmt = iabdSummary.amount
        amount = MoneyPounds(rawAmt, 0).quantity

        unTaxedInterestData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (unTaxedInterestData)


    val foreignInterestRows: List[Option[(String, String, String)]] =
      for {
        iabdSummary <- foreignInterest.iabdSummaries

        message: (String, String) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"))
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "")
          }
        }
        rawAmt = iabdSummary.amount
        amount = MoneyPounds(rawAmt, 0).quantity

        foreignInterestData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (foreignInterestData)



    val foreignDividendsRows: List[Option[(String, String, String)]] =
      for {
        iabdSummary <- foreignDividends.iabdSummaries

        message: (String, String) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"))
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "")
          }
        }
        rawAmt = iabdSummary.amount
        amount = MoneyPounds(rawAmt, 0).quantity

        foreignDividendsData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (foreignDividendsData)


    val allInvestmentIncomeRows = dividendsRows ::: bankInterestRows ::: unTaxedInterestRows ::: foreignInterestRows ::: foreignDividendsRows
    (allInvestmentIncomeRows.flatten, totalInvestmentIncome)
  }
//
  //temp fix to move Bereavement allowance into taxable state benefit table remove/refactor after data-structure change
  def createTaxableBenefitTable(nonTaxCodeIncomes: Option[NoneTaxCodeIncomes], taxCodeIncomes: Option[TaxCodeIncomes], bevAllowance:Option[IabdSummary] = None): (List[(String, String, String)], BigDecimal) = {

    val taxableStateBenefit = {
      val tStateBenefit = nonTaxCodeIncomes.map(_.taxableStateBenefit).flatten

      if (tStateBenefit.isDefined)
        tStateBenefit.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())
    }

    val taxableStateBenefitIncomes = taxCodeIncomes.map(_.taxableStateBenefitIncomes).flatten
    val taxCodeIncome = taxableStateBenefitIncomes.map(_.taxCodeIncomes).getOrElse(List())
    val taxableStateBenefitEmploymentRows: List[(Option[(String, String, String)], scala.BigDecimal)] =
      for {
        payeIncome <- taxCodeIncome

        message: (String) = {
          (payeIncome.name)
        }

        rawAmt = payeIncome.income.getOrElse(BigDecimal(0))
        amount = MoneyPounds(rawAmt, 0).quantity

        taxableStateBenefitData = if (rawAmt > 0)
          Some(message, amount, "")
        else
          None

      } yield (taxableStateBenefitData, rawAmt)

    val unzipTaxableStateBenEmpRows = taxableStateBenefitEmploymentRows.unzip
    val statePension = nonTaxCodeIncomes.map(_.statePension).flatten.getOrElse(BigDecimal(0))
    val statePensionLumpSum = nonTaxCodeIncomes.map(_.statePensionLumpSum).flatten.getOrElse(BigDecimal(0))

    val statePensionData = if (statePension > 0)
      Some(Messages("tai.income.statePension.title"), MoneyPounds(statePension, 0).quantity, Messages("tai.iabdSummary.description-state-pension"))
    else
      None

    val statePensionLumpSumData = if (statePensionLumpSum > 0)
      Some(Messages("tai.income.statePensionLumpSum.total"), MoneyPounds(statePensionLumpSum, 0).quantity, "")
    else
      None

    val tStateBenefitRows: List[Option[(String, String, String)]] =
      for {
        iabdSummary <- taxableStateBenefit.iabdSummaries

        message: (String, String) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"))
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "")
          }

        }

        rawAmt = iabdSummary.amount
        amount = MoneyPounds(rawAmt, 0).quantity

        tStateBenefitData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (tStateBenefitData)

    val totalBenefits: BigDecimal = statePension +
      statePensionLumpSum + taxableStateBenefit.amount + unzipTaxableStateBenEmpRows._2.sum + bevAllowance.map(_.amount).getOrElse(0)


    //need to put bev allowance description
    val bevAllowanceRow = if (bevAllowance.isDefined)
      Some(Messages("tai.iabdSummary.type-125"),
        MoneyPounds(bevAllowance.map(_.amount).getOrElse(0), 0).quantity,
        Messages(""))
    else None

    val statePensionRows = List(statePensionData, statePensionLumpSumData, bevAllowanceRow)

    val allOtherIncomeRows = tStateBenefitRows ::: unzipTaxableStateBenEmpRows._1 ::: statePensionRows

    (allOtherIncomeRows.flatten, totalBenefits)

  }
//
  def createOtherIncomeTable(nonTaxCodeIncomes: Option[NoneTaxCodeIncomes], otherIncome: List[IabdSummary]): (List[(String, String, String)], BigDecimal) = {

    val otherIncomeRows: List[Option[(String, String, String)]] =
      for {
        iabdSummary <- otherIncome
        message: (String, String, Boolean) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"),
              true)
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "",
              true)
          }
        }

        rawAmt = iabdSummary.amount
        amount :String = MoneyPounds(rawAmt, 0).quantity

        otherIncomeData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (otherIncomeData)


    val otherPension = {
      val otherPensions = nonTaxCodeIncomes.map(_.otherPensions).flatten

      if (otherPensions.isDefined)
        otherPensions.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())

    }

    val otherPensionRows =
      for {
        iabdSummary <- otherPension.iabdSummaries

        message: (String, String, Boolean) = {
          if (!Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}").isEmpty) {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), Messages(s"tai.iabdSummary.type-${iabdSummary.iabdType}"),
              true)
          }
          else {
            (Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"), "",
              true)
          }

        }

        rawAmt = iabdSummary.amount
        amount :String = MoneyPounds(rawAmt, 0).quantity

        otherPensionData = if (rawAmt > 0)
          Some((message._2, amount, message._1))
        else
          None

      } yield (otherPensionData)

    val totalOtherIncomeAmount = otherIncome.map(_.amount).sum + otherPension.amount

    val allOtherIncomeRows = otherIncomeRows ::: otherPensionRows

    (allOtherIncomeRows.flatten, totalOtherIncomeAmount)

  }
//
  def createBenefitsTable(benefitsFromEmployment: TaxComponent): (List[(String, String, String, String, Option[Int], Option[Int])], BigDecimal) = {

    val benefitsRows: List[Option[(String, String, String, String, Option[Int], Option[Int])]] =
      for {iabdSummary <- benefitsFromEmployment.iabdSummaries

        message: (String, String, String) = iabdSummary.iabdType match {
          case 30 => {
// TODO...drop the URL's from the response. Drop once discussed with integration.
            (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}", iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
              "SOME URL")//config.ApplicationConfig.medBenefitServiceUrl)
          }
          case 31 => {
            (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}", iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
              "SOME URL")//config.ApplicationConfig.companyCarServiceUrl)
          }
          case _ => {
            (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}", iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
              "")
          }
        }

        rawAmt = iabdSummary.amount
        amount = MoneyPounds(rawAmt, 0).quantity

        benefitsData = if (rawAmt > 0)
          Some(message._1, amount, message._2, message._3, iabdSummary.employmentId, Some(iabdSummary.iabdType))
        else
          None

      } yield (benefitsData)

    val totalBenefitsAmount = benefitsFromEmployment.amount
    (benefitsRows.flatten, totalBenefitsAmount)
  }
//
  //method to decide if the user can see info about cy+1 or not. Can be updated to include Gatekeeper rules
  def cyPlusOneAvailable(taxSummaryDetals: TaxSummaryDetails):Boolean = {taxSummaryDetals.cyPlusOneChange.isDefined}


  def getPotentialUnderpayment(taxDetails : TaxSummaryDetails) :Option[BigDecimal] = {
    val incomesWithUnderpayment = taxDetails.increasesTax.flatMap(_.incomes.map(incomes =>
      TaxSummaryHelper.sortedTaxableIncomes(incomes.taxCodeIncomes).filter(_.tax.totalInYearAdjustment.isDefined))).getOrElse(Nil)

    incomesWithUnderpayment.foldLeft(BigDecimal(0))((total,income) =>
      income.tax.totalInYearAdjustment.getOrElse(BigDecimal(0))  + total)
    match {
      case x if (x>0) => Some(x)
      case _ => None
    }
  }

}
