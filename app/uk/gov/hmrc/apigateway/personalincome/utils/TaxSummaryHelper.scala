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

package uk.gov.hmrc.apigateway.personalincome.utils

import org.joda.time.DateTime
import play.Logger
import play.api.i18n.Messages
import uk.gov.hmrc.apigateway.personalincome.domain._
import uk.gov.hmrc.play.views.helpers.MoneyPounds


object TaxSummaryHelper {


  def sortedTaxableIncomes(incomes: TaxCodeIncomes) = {
    val taxableIncomes = incomes.employments.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.occupationalPensions.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.taxableStateBenefitIncomes.map(_.taxCodeIncomes).getOrElse(Nil) :::
      incomes.ceasedEmployments.map(_.taxCodeIncomes).getOrElse(Nil)

    taxableIncomes.sortBy(employment => (employment.employmentType.getOrElse(0) * 1000) + employment.employmentId.getOrElse(0))
  }
//
//
//  def mergeItems(curr : List[CompareItem], next: List[CompareItem]) : List[CompareItem] = {
//    (curr ::: next).groupBy(x => (x.label,x.secondarySort)).mapValues{list =>
//      if(list.size == 2){
//        List(CompareItem(label = list(0).label, from = list(0).from, to = list(1).to))
//      }else{
//        list
//      }
//    }.map(_._2).toList.flatten
//  }
//
//  def getEditableIncomes(incomes: Option[Incomes]): List[TaxCodeIncomeSummary] = {
//    val allIncomes = incomes.map { payeIncomes =>
//      sortedTaxableIncomes(payeIncomes.taxCodeIncomes)
//    }
//    allIncomes.map(_.filter(_.isEditable)).getOrElse(Nil)
//  }
//
//  def getNonEditableIncomes(incomes: Option[Incomes]): List[TaxCodeIncomeSummary] = {
//    val allIncomes = incomes.map { payeIncomes =>
//      sortedTaxableIncomes(payeIncomes.taxCodeIncomes)
//    }
//    allIncomes.map(_.filter(_.isEditable == false)).getOrElse(Nil)
//  }
//
//  def isIncomeEditable(incomes: Option[Incomes]): Boolean = {
//    !getEditableIncomes(incomes).isEmpty
//  }
//
//  def getPALimit(decreasesTax: Option[DecreasesTax]): BigDecimal = {
//    (decreasesTax.map(_.personalAllowanceSourceAmount.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0)) * 2) + 100000
//  }
//
  def getActualPA(decreasesTax: Option[DecreasesTax]): BigDecimal = {
    decreasesTax.map(_.personalAllowance.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0))
  }
//
//  def isAfterMarchPayRoll(currentDate: DateTime): Boolean = {
//    val currentYear = new DateTime()
//    val startDate = new DateTime(currentYear.year().get(), 3, 13, 0, 0, 0, 0).minusSeconds(1)
//    val endDate = new DateTime(currentYear.year().get(), 4, 6, 0, 0, 0, 0)
//    (currentDate.isAfter(startDate) && currentDate.isBefore(endDate))
//  }
//
//
//  def getIncomeSelectorDefaultIncomeMessage(incomes: Incomes): Option[String] = {
//    val PRIMARY_INCOME = 1
//    val SECONDARY_INCOME = 2
//    val PRIMARY_INCOME_DEFAULT = 15000
//    val SECONDARY_INCOME_DEFAULT = 5000
//
//    val groupedIncomes = incomes.taxCodeIncomes.employments.map(_.taxCodeIncomes).getOrElse(Nil) :::
//      incomes.taxCodeIncomes.occupationalPensions.map(_.taxCodeIncomes).getOrElse(Nil) :::
//      incomes.taxCodeIncomes.ceasedEmployments.map(_.taxCodeIncomes).getOrElse(Nil)
//
//    val primaryDefault = groupedIncomes.find(taxCodeIncome => taxCodeIncome.employmentType == Some(PRIMARY_INCOME)
//      && taxCodeIncome.income == Some(BigDecimal(PRIMARY_INCOME_DEFAULT)))
//    val secondaryDefault = groupedIncomes.find(taxCodeIncome => taxCodeIncome.employmentType == Some(SECONDARY_INCOME)
//      && taxCodeIncome.income == Some(BigDecimal(SECONDARY_INCOME_DEFAULT)))
//
//    (primaryDefault.isDefined, secondaryDefault.isDefined) match {
//      case (true, false) => Some(Messages("tai.incomes.primaryDefault", MoneyPounds(PRIMARY_INCOME_DEFAULT, 0).quantity))
//      case (false, true) => Some(Messages("tai.incomes.secondaryDefault", MoneyPounds(SECONDARY_INCOME_DEFAULT, 0).quantity))
//      case (true, true) => Some(Messages("tai.incomes.priAndSecDefault", MoneyPounds(PRIMARY_INCOME_DEFAULT, 0).quantity,
//        MoneyPounds(SECONDARY_INCOME_DEFAULT, 0).quantity))
//      case _ => None
//    }
//  }
//
//  def getIncomeSelectorScenarioMessage(incomes: Incomes): Option[String] = {
//    val editableIncomes = getEditableIncomes(Some(incomes))
//
//    val occupationPension = editableIncomes.find(_.isOccupationalPension)
//    val liveIncome = editableIncomes.find(income => !income.isOccupationalPension && income.isLive)
//    val ceasedIncome = editableIncomes.find(income => !income.isOccupationalPension && !income.isLive)
//
//    (liveIncome.isDefined, ceasedIncome.isDefined) match {
//      case (true, false) => Some(Messages("tai.incomes.select.scenario1"))
//      case (false, true) => Some(Messages("tai.incomes.select.scenario2"))
//      case (true, true) => Some(Messages("tai.incomes.select.scenario1and2"))
//      case _ => None
//    }
//  }
//
//  def getIncomeBeforeTaxMessage(incomes: Option[Incomes], gateKeepered: Boolean = false): String = {
//
//    val editableIncomes = getEditableIncomes(incomes)
//
//    val nonEditableIncomes = getNonEditableIncomes(incomes)
//    val noneditableCeasedIncomes = nonEditableIncomes.filter { income => !income.isOccupationalPension && !income.isLive}
//    val editableCeasedIncomes = editableIncomes.filter { income => !income.isOccupationalPension && !income.isLive}
//    val allCeasedIncomes = incomes.map(_.taxCodeIncomes.ceasedEmployments.isDefined).getOrElse(false)
//
//    val hasLiveEmployment = incomes.map(_.taxCodeIncomes.employments.isDefined).getOrElse(false)
//    val hasPensions = incomes.map(_.taxCodeIncomes.occupationalPensions.isDefined).getOrElse(false)
//    val hasCeasedEditable = editableCeasedIncomes.filter(_.isEditable).size > 0
//    val hasCeasedNonEditable = noneditableCeasedIncomes.find(_.isEditable == false).size > 0
//
//    if (gateKeepered == false) {
//      (hasLiveEmployment, hasPensions, hasCeasedEditable, hasCeasedNonEditable) match {
//        case (true, true, true, true) => Messages("tai.viewIncomes.text.everything")
//        case (true, true, true, false) => Messages("tai.viewIncomes.text.incomeAndPension")
//        case (true, true, false, false) => Messages("tai.viewIncomes.text.incomeAndPension")
//        case (true, false, false, false) => Messages("tai.viewIncomes.text.incomeOnly")
//        case (true, false, true, false) => Messages("tai.viewIncomes.text.incomeOnly")
//        case (true, false, false, true) => Messages("tai.viewIncomes.text.nonEditableCeasedAndIncome")
//        case (true, true, false, true) => Messages("tai.viewIncomes.text.everything")
//        case (true, false, true, true) => Messages("tai.viewIncomes.text.nonEditableCeasedAndIncome")
//        case (false, true, true, true) => Messages("tai.viewIncomes.text.everything")
//        case (false, true, true, false) => Messages("tai.viewIncomes.text.incomeAndPension")
//        case (false, true, false, true) => Messages("tai.viewIncomes.text.nonEditableCeasedAndPension")
//        case (false, true, false, false) => Messages("tai.viewIncomes.text.pensionOnly")
//        case (false, false, true, true) => Messages("tai.viewIncomes.text.nonEditableCeasedAndIncome")
//        case (false, false, true, false) => Messages("tai.viewIncomes.text.incomeOnly")
//        case (false, false, false, true) => Messages("tai.viewIncomes.text.nonEditableCeasedOnly")
//        case _ => ""
//
//      }
//    } else {
//      (hasLiveEmployment, hasPensions, hasCeasedEditable, hasCeasedNonEditable) match {
//        case (true, true, true, true) => Messages("tai.viewIncomes.text.gk.everything")
//        case (true, true, true, false) => Messages("tai.viewIncomes.text.gk.incomeAndPension")
//        case (true, true, false, false) => Messages("tai.viewIncomes.text.gk.incomeAndPension")
//        case (true, false, false, false) => Messages("tai.viewIncomes.text.gk.incomeOnly")
//        case (true, false, true, false) => Messages("tai.viewIncomes.text.gk.incomeOnly")
//        case (true, false, false, true) => Messages("tai.viewIncomes.text.gk.nonEditableCeasedAndIncome")
//        case (true, true, false, true) => Messages("tai.viewIncomes.text.gk.everything")
//        case (true, false, true, true) => Messages("tai.viewIncomes.text.gk.nonEditableCeasedAndIncome")
//        case (false, true, true, true) => Messages("tai.viewIncomes.text.gk.everything")
//        case (false, true, true, false) => Messages("tai.viewIncomes.text.gk.incomeAndPension")
//        case (false, true, false, true) => Messages("tai.viewIncomes.text.gk.nonEditableCeasedAndPension")
//        case (false, true, false, false) => Messages("tai.viewIncomes.text.gk.pensionOnly")
//        case (false, false, true, true) => Messages("tai.viewIncomes.text.gk.nonEditableCeasedAndIncome")
//        case (false, false, true, false) => Messages("tai.viewIncomes.text.gk.incomeOnly")
//        case (false, false, false, true) => Messages("tai.viewIncomes.text.gk.nonEditableCeasedOnly")
//        case _ => ""
//      }
//    }
//
//  }
//
//  def isPersonalAllowanceTapered(decreasesTax: DecreasesTax) = {
//    if (decreasesTax.personalAllowanceSourceAmount.isDefined && decreasesTax.personalAllowance.isDefined) {
//      (decreasesTax.personalAllowance.getOrElse(BigDecimal(0)).<(decreasesTax.personalAllowanceSourceAmount.getOrElse(BigDecimal(0))))
//    } else {
//      false
//    }
//  }
//
//  def getStepNumber(currentPage: Int, hasMultipleIncomes: Boolean) = {
//    if (hasMultipleIncomes == false) {
//      Messages("tai.step", currentPage - 1, 3)
//    } else {
//      Messages("tai.step", currentPage, 4)
//    }
//  }
//
//  def getNumberOfLiabilities(liabilities: model.TotalLiability): Int = {
//    val liabilityItems = List(liabilities.totalTax, liabilities.childBenefitTaxDue, liabilities.underpaymentPreviousYear)
//    liabilityItems.filter(x => x != 0).size
//  }
//
//  def hasNoTaxableIncome(taxSummaryDetails: model.TaxSummaryDetails): Boolean = {
//    (taxSummaryDetails.increasesTax.map(_.total).getOrElse(BigDecimal(0)) > taxSummaryDetails.decreasesTax.map(_.total).getOrElse(BigDecimal(0)))
//  }
//
//  def getTaxRate(taxSummaryDetails: model.TaxSummaryDetails) = {
//    taxSummaryDetails.totalLiability.flatMap(_.nonSavings.flatMap(_.taxBands.flatMap(_.headOption.map(_.rate.getOrElse(BigDecimal(0)))))).getOrElse(BigDecimal(0))
//  }
//
//  def displayTaxOnInvestmentIncome(totalLiability: TotalLiability) = {
//
//    val taxList = List(
//      Some(totalLiability.childBenefitTaxDue),
//      Some(totalLiability.underpaymentPreviousYear)
//    ).flatten
//
//    val totalTax = taxList.fold(BigDecimal(0))(_ + _)
//    (totalTax > 0)
//  }
//
//  def displayTaxPaidElsewhere(totalLiability: TotalLiability) = {
//
//    val taxList = List(
//      totalLiability.ukDividends.flatMap(_.actualTaxDueAssumingAllAtBasicRate),
//      totalLiability.nonCodedIncome.flatMap(_.totalTax),
//      totalLiability.bankInterest.flatMap(_.actualTaxDueAssumingAllAtBasicRate)
//    ).flatten
//
//    val totalTax = taxList.fold(BigDecimal(0))(_ + _)
//    (totalTax > 0)
//  }


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
//
//  def getGateKeeperMessage(gateKeeperRule: GateKeeperRule) = {
//    Logger.warn(s" ${gateKeeperRule.gateKeeperType}, ${gateKeeperRule.id} : ${gateKeeperRule.description}")
//    val messageNumber = "tai.gatekeeper." + gateKeeperRule.gateKeeperType + "." + gateKeeperRule.id
//    Messages(messageNumber)
//  }
//
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
//
//  def hasMultipleIncomes(details: model.TaxSummaryDetails): Boolean = {
//    getEditableIncomes(details.increasesTax.flatMap(_.incomes)).size > 1
//  }
//
//  def getTaxablePayYTD(details: model.TaxSummaryDetails, employerId: BigDecimal): BigDecimal = {
//    val incomeExplanations = details.incomeData.map(x => x.incomeExplanations)
//
//    val taxablePayYTD: BigDecimal = incomeExplanations match {
//      case Some(incomeExplanations) => {
//        val income = incomeExplanations.filter(_.incomeId == employerId).headOption
//        income.map(_.payToDate).getOrElse(BigDecimal(0))
//      }
//      case _ => BigDecimal(0)
//    }
//    taxablePayYTD
//  }
//
//  def getSingularIncomeId(details: model.TaxSummaryDetails): Option[Int] = {
//    val editableIncomes = getEditableIncomes(details.increasesTax.flatMap(_.incomes))
//    if (editableIncomes.size == 1) {
//      editableIncomes.flatMap { income =>
//        income.employmentId
//      }.headOption
//    } else {
//      None
//    }
//  }
//
//  def getGateKeeperReasonList(rules: List[GateKeeperRule]): List[String] = {
//    rules.map { rule =>
//      val messageNumber = "tai.gatekeeper." + rule.gateKeeperType + "." + rule.id
//      Messages(messageNumber)
//    }.distinct
//  }
//
//  def hasOnlyCeasedIncome(details: TaxCodeIncomes): Boolean = {
//    !details.employments.isDefined && !details.occupationalPensions.isDefined && !details.taxableStateBenefitIncomes.isDefined
//  }
//
  def removeDecimalsToString(decimal: math.BigDecimal): String = {
    decimal.bigDecimal.stripTrailingZeros.toPlainString
  }
//
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
          Some(message._2, amount, message._1)
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
          Some(message._2, amount, message._1)
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
          Some(message._2, amount, message._1)
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
          Some(message._2, amount, message._1)
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
          Some(message._2, amount, message._1)
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
          Some(message._2, amount, message._1)
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
          Some(message._2, amount, message._1)
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
          Some(message._2, amount, message._1)
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
//
//  def createTaxCodesTable(deductions: List[TaxCodeComponent], allowances: List[TaxCodeComponent], benefitsFromEmployment: TaxComponent, childBenefitTaxDue: BigDecimal,
//  outstandingDebt: BigDecimal, underpayment: BigDecimal, total: BigDecimal, deductionsReceived: Boolean): List[(String, String, String, String,  Option[Int], Option[Int]   )] = {
//
//      val deductionsFiltered: List[TaxCodeComponent] = deductions.filter(taxComponent =>  (taxComponent.componentType.getOrElse(0) != 7))
//    val taxCodesDeductionRows: List[Option[(String, String, String, String, Option[Int], Option[Int])]] =
//    for {
//    deduction <- deductionsFiltered
//    componentType = deduction.componentType.getOrElse(0)
//    message: (String, String, Boolean) = componentType match {
//        case 1 => {
//            (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-1.desc"),
//          true)
//        }
//        case 5 => {
//            (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-5.desc"),
//          true)
//        }
//        case 27 => {
//            (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-27.desc"),
//          true)
//        }
//        case 23 => {
//            (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-23.desc"),
//              true)
//        }
//        case 32 => {
//            (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-32.desc"),
//              true)
//        }
//        case 33 => {
//          (Messages(s"tai.taxcode.deduction.type-${componentType}"), "",
//            true)
//        }
//        case 35 => {
//            (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.underpayment.description", MoneyPounds(underpayment, 2).quantity),
//          true)
//        }
//        case 38 => {
//          (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-38.desc"),
//            true)
//        }
//        case 40 => {
//              (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-40.desc"),
//            true)
//        }
//        case 41 => {
//              (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.outstanding.debt.description", MoneyPounds(outstandingDebt, 2).quantity),
//            true)
//        }
//        case 42 => {
//              (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.child.benefit.description", MoneyPounds(childBenefitTaxDue, 2).quantity),
//            true)
//        }
//        case 44 => {
//          (Messages(s"tai.taxcode.deduction.type-${componentType}"), Messages("tai.taxcode.deduction.type-44.desc"),
//            true)
//        }
//        case _ => {
//            (Messages(s"tai.taxcode.deduction.type-${componentType}"), "",
//              false)
//        }
//      }
//
//        rawAmt = deduction.amount.get
//        amount = "-"+MoneyPounds(rawAmt, 0).quantity
//
//        taxCodesDeductionData = if (rawAmt > 0)
//          Some(message._1, amount, message._2, "", None, None)
//        else
//          None
//
//      } yield (taxCodesDeductionData)
//
//    val benefitsRows: List[Option[(String, String, String, String, Option[Int], Option[Int])]] =
//      for {
//        iabdSummary <- benefitsFromEmployment.iabdSummaries
//
//        message: (String, String, String) = iabdSummary.iabdType match {
//          case 30 => {
//            (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}", iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
//              config.ApplicationConfig.medBenefitServiceUrl)
//          }
//          case 31 => {
//            (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}", iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
//              config.ApplicationConfig.companyCarServiceUrl)
//          }
//          case _ => {
//            (Messages(s"tai.iabdSummary.employmentBenefit.type-${iabdSummary.iabdType}", iabdSummary.employmentName.getOrElse("")), Messages(s"tai.iabdSummary.description-${iabdSummary.iabdType}"),
//              "")
//          }
//        }
//
//        rawAmt = iabdSummary.amount
//        amount = "-"+MoneyPounds(rawAmt, 0).quantity
//
//        benefitsData = if (rawAmt > 0)
//          Some(message._1, amount, message._2, message._3, iabdSummary.employmentId, Some(iabdSummary.iabdType))
//        else
//          None
//
//      } yield (benefitsData)
//
//
//    val taxCodesAllowanceRows =
//      for {
//        allowance <- allowances
//        componentType = allowance.componentType.getOrElse(0)
//        message: (String, String, Boolean) = componentType match {
//          case 5 => {
//            (Messages(s"tai.taxcode.allowance.type-${componentType}"),
//              Messages("tai.taxcode.allowance.description"),
//              true)
//          }
//          case 6 => {
//            (Messages(s"tai.taxcode.allowance.type-${componentType}"),
//              Messages("tai.taxcode.allowance.description"),
//              true)
//          }
//          case 10 | 7 | 28 | 29 => {
//            (Messages(s"tai.taxcode.allowance.type-${componentType}"),
//              Messages(s"tai.taxcode.allowance.type-${componentType}.desc"),
//              true)
//          }
//          case 15 | 16 | 17 | 18 | 21 => {
//            (Messages(s"tai.taxcode.allowance.type-${componentType}"),
//              Messages("tai.taxcode.allowance.type-15.desc",
//                routes.EstimatedIncomeTaxController.estimatedIncomeTax()),
//              true)
//          }
//          case _ => {
//            (Messages(s"tai.taxcode.allowance.type-${componentType}"), "",
//              false)
//          }
//        }
//        taxCodesAllowanceData = if (allowance.amount.isDefined) {
//          val rawAmt = allowance.amount.get
//          val amount = MoneyPounds(rawAmt, 0).quantity
//          Some(message._1, amount, message._2, "", None, None)
//        } else
//          None
//
//      } yield (taxCodesAllowanceData)
//
//    val taxCodesRows = taxCodesAllowanceRows ::: taxCodesDeductionRows ::: benefitsRows
//    taxCodesRows.flatten
//  }
//
//  object dates {
//    private val londonTimeZone = TimeZone.getTimeZone("Europe/London")
//
//    private def easyReadingTimeStampFormat = new SimpleDateFormat("h:mmaa")
//
//    private def easyReadingDateFormat = new SimpleDateFormat("EEEE d MMMM yyyy")
//
//    def formatEasyReadingTimeStamp(date: Option[DateTime], default: String) =
//      date match {
//        case Some(d) =>
//          val time = easyReadingTimeStampFormat.format(d.toDate).toLowerCase
//          val date = easyReadingDateFormat.format(d.toDate)
//          s"$time, $date"
//        case None => default
//      }
//  }

//  implicit def strToMoneyPounds(str: String): MoneyPounds = {
//    MoneyPounds(BigDecimal(str))
//  }
}
