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

package uk.gov.hmrc.personalincome.domain

import play.api.Play.current
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.model._
import uk.gov.hmrc.personalincome.utils.{TaxDecorator, TaxSummaryHelper}
import uk.gov.hmrc.play.views.helpers.MoneyPounds


object BaseViewModelVM extends ViewModelFactory {
  override type ViewModelType = BaseViewModel

  override def createObject(nino: Nino, details: TaxSummaryDetails) : BaseViewModel = {
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

    BaseViewModel(incomeTax, taxableIncome, taxFreeAmount, personalAllowance, hasTamc, taxCodeEmploymentList, details.cyPlusOneChange.isDefined)
  }
}

object EstimatedIncomePageVM extends ViewModelFactory {
  override type ViewModelType = EstimatedIncome
  override def createObject(nino:Nino, details: TaxSummaryDetails): EstimatedIncome = {

    val incTax:Boolean = details.increasesTax match {
      case Some(increasesTax) => true
      case _ => false
    }
    val estimatedTotalTax:BigDecimal = details.totalLiability.get.totalTax
    val decTaxTotal:BigDecimal = details.decreasesTax.get.total
    val incTotal:BigDecimal = details.increasesTax.get.total

    val reliefs:Boolean = TaxSummaryHelper.getPPR(details)._1 > BigDecimal(0) || TaxSummaryHelper.getGiftAid(details)._1 > BigDecimal(0)
    val emps = details.taxCodeDetails.flatMap(_.employment)
    val listEmps = emps match
    {
      case Some(list) => for( e <- list) yield {e.taxCode.get}
      case _ => List[String]()
    }

    val potentialUnderpayment = {
      val incomesWithUnderpayment = details.increasesTax.flatMap(_.incomes.map(incomes =>
        TaxSummaryHelper.sortedTaxableIncomes(incomes.taxCodeIncomes).filter(_.tax.potentialUnderpayment.isDefined))).getOrElse(Nil)
      incomesWithUnderpayment.foldLeft(BigDecimal(0))((total,income) =>
        income.tax.potentialUnderpayment.getOrElse(BigDecimal(0))  + total)
      match {
        case x if x >0 => true
        case _ => false
      }
    }

    val totalLiability = details.totalLiability.get
    val underPayment = if (totalLiability.underpaymentPreviousYear > 0)
      Some(MessageWrapper(Messages("tai.taxCalc.UnderpaymentPreviousYear.title"),
        MoneyPounds(totalLiability.underpaymentPreviousYear,2).quantity)
      ) else None

    val childBenefitTax = if (totalLiability.childBenefitTaxDue > 0)
      Some(MessageWrapper(Messages("tai.taxCalc.childBenefit.title"),
        MoneyPounds(totalLiability.childBenefitTaxDue,2).quantity)
      ) else None

    val outStandingDebt = if (totalLiability.outstandingDebt > 0)
      Some(MessageWrapper(Messages("tai.taxCalc.OutstandingDebt.title"),
        MoneyPounds(totalLiability.outstandingDebt,2).quantity)
      ) else None


    val excessGiftAidTax = totalLiability.liabilityAdditions.flatMap(_.excessGiftAidTax.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val excessGiftAidTaxMessage = if (excessGiftAidTax > 0)
      Some(MessageWrapper(Messages("tai.taxCalc.excessGiftAidTax.title"),
        MoneyPounds(excessGiftAidTax,2).quantity)
      ) else None


    val excessWidowsAndOrphans = totalLiability.liabilityAdditions.flatMap(_.excessWidowsAndOrphans.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val excessWidowsAndOrphansMessage = if (excessWidowsAndOrphans > 0)
      Some(MessageWrapper(Messages("tai.taxCalc.excessWidowsAndOrphans.title"),
        MoneyPounds(excessWidowsAndOrphans,2).quantity)
      ) else None


    val pensionPaymentsAdjustment = totalLiability.liabilityAdditions.flatMap(_.pensionPaymentsAdjustment.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val pensionPaymentsAdjustmentMessage = if (pensionPaymentsAdjustment > 0)
      Some(MessageWrapper(Messages("tai.taxCalc.pensionPaymentsAdjustment.title"),
        MoneyPounds(pensionPaymentsAdjustment,2).quantity)
      ) else None

    val additionalTable = List(underPayment,childBenefitTax,outStandingDebt,excessGiftAidTaxMessage,excessWidowsAndOrphansMessage,pensionPaymentsAdjustmentMessage).flatten
    val additionalTableTotal = MoneyPounds(TaxSummaryHelper.getTotalAdditionalTaxDue(totalLiability),2).quantity

    val nci = totalLiability.nonCodedIncome.map(_.totalTax.getOrElse(BigDecimal(0))).getOrElse(BigDecimal(0))
    val nonCodedIncome = if (nci > 0)
      Some(MessageWrapper(Messages("tai.taxCollected.atSource.otherIncome.title"),
        "-"+MoneyPounds(nci,2).quantity,
        Some(Messages("tai.taxCollected.atSource.otherIncome.description")))
      ) else None

    val ukd = totalLiability.taxCreditOnUKDividends.getOrElse(BigDecimal(0))
    val ukDividends = if (ukd > 0)
      Some(MessageWrapper(Messages("tai.taxCollected.atSource.dividends.title"),
        "-"+MoneyPounds(ukd,2).quantity,
        Some(Messages("tai.taxCollected.atSource.dividends.description",10)))
      ) else None

    val interest = totalLiability.taxOnBankBSInterest.getOrElse(BigDecimal(0))

    val bankInterest = if (interest > 0)
      Some(MessageWrapper(Messages("tai.taxCollected.atSource.bank.title"),
        "-"+MoneyPounds(interest,2).quantity,
        Some(Messages("tai.taxCollected.atSource.bank.description",20)))
      ) else None

    val maNet = totalLiability.liabilityReductions.flatMap(_.marriageAllowance.map(_.marriageAllowanceRelief)).getOrElse(BigDecimal(0))
    val maGross = totalLiability.liabilityReductions.flatMap(_.marriageAllowance.map(_.marriageAllowance)).getOrElse(BigDecimal(0))
    val maValue = if (maNet > 0) {
      val a = Messages("tai.taxCollected.atSource.marriageAllowance.title")
      val b = "-"+MoneyPounds(maNet,2).quantity

      // TODO...
      //val c =   Messages("tai.taxCollected.atSource.marriageAllowance.description", MoneyPounds(maGross).quantity, routes.YourTaxCodeController.viewTaxCode())
      val c =   Messages("tai.taxCollected.atSource.marriageAllowance.description", MoneyPounds(maGross).quantity, "TODO...")
      Some(MessageWrapper(a,b,Some(c)))
    } else None

    val mp = totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val mpCoding = totalLiability.liabilityReductions.flatMap(_.maintenancePayments.map(_.codingAmount)).getOrElse(BigDecimal(0))

    // TODO...
    val maint =  if(mp > 0){
      Some(MessageWrapper(Messages("tai.taxCollected.atSource.maintenancePayments.title"),"-"+MoneyPounds(mp).quantity,
        Some(Messages("tai.taxCollected.atSource.maintenancePayments.description", MoneyPounds(mpCoding).quantity, "TODO...")))
      )}else{None}

    val enterpriseInvestmentScheme = totalLiability.liabilityReductions.flatMap(
      _.enterpriseInvestmentSchemeRelief.map(
        _.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val enterpriseInvestmentSchemeRelief =  if(enterpriseInvestmentScheme > 0){Some(
      MessageWrapper(Messages("tai.taxCollected.atSource.enterpriseInvestmentSchemeRelief.title"),
        "-"+MoneyPounds(enterpriseInvestmentScheme).quantity,
        Some(Messages("tai.taxCollected.atSource.enterpriseInvestmentSchemeRelief.description"))))} else{None}

    val cr = totalLiability.liabilityReductions.flatMap(_.concessionalRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val concessionalRelief =  if(enterpriseInvestmentScheme > 0){Some(MessageWrapper(
      Messages("tai.taxCollected.atSource.concessionalRelief.title"),
      "-"+MoneyPounds(cr).quantity,
      Some(Messages("tai.taxCollected.atSource.concessionalRelief.description"))))}else{None}

    val dtr = totalLiability.liabilityReductions.flatMap(_.doubleTaxationRelief.map(_.amountInTermsOfTax)).getOrElse(BigDecimal(0))
    val doubleTaxationRelief =  if(enterpriseInvestmentScheme > 0) {
      Some(MessageWrapper(Messages("tai.taxCollected.atSource.doubleTaxationRelief.title"),
        "-"+MoneyPounds(dtr).quantity,
        Some(Messages("tai.taxCollected.atSource.doubleTaxationRelief.description"))))}else{None}


    val reductionsTable = List(nonCodedIncome,ukDividends,bankInterest, maValue, maint,enterpriseInvestmentSchemeRelief,concessionalRelief,doubleTaxationRelief).flatten
    val reductionsTableTotal = "-"+MoneyPounds(TaxSummaryHelper.getTotalReductions(totalLiability),2).quantity

    val mergedIncome = totalLiability.mergedIncomes.getOrElse(throw new RuntimeException("No data"))
    val td = TaxDecorator(mergedIncome, paAmount = TaxSummaryHelper.getActualPA(details.decreasesTax))
    val bands = for {
      tb <- td.taxBandDescriptions
    } yield Band(
        tb.className,
        tb.widthAsPerc,
        TaxSummaryHelper.removeDecimalsToString(tb.taxBand.rate.getOrElse(BigDecimal(0))),
        tb.taxBand.income.getOrElse(BigDecimal(0)),
        tb.taxBand.tax.getOrElse(BigDecimal(0))
      )
    val incomeAsPerc = td.incomeAsPercentage
    val graph = BandedGraph("taxGraph",bands,td.taxBandLabelFirstAmount,td.taxBandLabelLastAmount,
      td.totalIncome,incomeAsPerc,td.taxBandDecorator.totalTax)


    EstimatedIncome(incTax,estimatedTotalTax,incTotal,decTaxTotal,reliefs,
      listEmps,potentialUnderpayment,additionalTable,additionalTableTotal,
      reductionsTable,reductionsTableTotal,graph,  TaxSummaryHelper.cyPlusOneAvailable(details))

  }
}

object YourTaxableIncomePageVM extends ViewModelFactory {
  override type ViewModelType = TaxableIncome
  override def createObject(nino: Nino, details: TaxSummaryDetails): TaxableIncome = {

    val increasesTax: Option[IncreasesTax] = details.increasesTax
    val totalLiability: Option[TotalLiability] = details.totalLiability

    val incomeTax: BigDecimal = totalLiability.map(_.totalTax).getOrElse(BigDecimal(0))
    val income: BigDecimal = increasesTax.map(_.total).getOrElse(BigDecimal(0))
    val taxFreeAmount: BigDecimal = details.decreasesTax.map(_.total).getOrElse(BigDecimal(0))
    val taxCodeEmploymentList = details.taxCodeDetails.flatMap(_.employment)
    val taxCodeList = taxCodeEmploymentList match {
      case Some(list) => for (e <- list) yield {
        e.taxCode.get
      }
      case _ => List[String]()
    }

    val incomes = details.increasesTax.flatMap(incTax => incTax.incomes)
    val taxCodeIncomes = incomes.map(inc => inc.taxCodeIncomes)
    val nonTaxCodeIncomes = incomes.map(inc => inc.noneTaxCodeIncomes)

    val investmentIncomeData = TaxSummaryHelper.createInvestmentIncomeTable(nonTaxCodeIncomes)
    val otherIncome = {
      val oIncome = nonTaxCodeIncomes.flatMap(_.otherIncome)

      if (oIncome.isDefined)
        oIncome.get
      else
        TaxComponent(BigDecimal(0), 0, "", List())
    }

    /* this is a temporary fix until data structure re-write, its purpose is to remove the finance iadbs from the
    other income table*/

    val financeDivIABDS = List(79,80,81,77,78,64,62)

    val otherIncomeIabds = otherIncome.iabdSummaries filter {
      iabd => !financeDivIABDS.contains(iabd.iabdType) && iabd.iabdType != 125
    }

    val otherIncomeData = TaxSummaryHelper.createOtherIncomeTable(nonTaxCodeIncomes, otherIncomeIabds)

    val employmentPension: (BigDecimal, Boolean, Boolean) = increasesTax.map { increasesTax =>
      val taxCodeIncomes = increasesTax.incomes.map(_.taxCodeIncomes)
      val employmentIncomes = taxCodeIncomes.flatMap(_.employments)
      val empIncome = employmentIncomes.map(_.taxCodeIncomes).getOrElse(List())
      val occupationalPensionIncomes = taxCodeIncomes.flatMap(_.occupationalPensions)
      val occPensionIncome = occupationalPensionIncomes.map(_.taxCodeIncomes).getOrElse(List())
      val ceasedIncomes = taxCodeIncomes.flatMap(_.ceasedEmployments)
      val ceasedIncome = ceasedIncomes.map(_.taxCodeIncomes).getOrElse(List())

      val empIncomeAmount = for {
        payeIncome <- empIncome
        amount = payeIncome.income.getOrElse(BigDecimal(0))
      } yield amount
      val empAmt = empIncomeAmount.sum

      val occPensionIncomeAmount = for {
        payeIncome <- occPensionIncome
        amount = payeIncome.income.getOrElse(BigDecimal(0))
      } yield amount
      val occPensionAmt = occPensionIncomeAmount.sum

      val ceasedIncomeAmount = for {
        payeIncome <- ceasedIncome

        amount = payeIncome.income.getOrElse(BigDecimal(0))
      } yield amount
      val ceasedAmt = ceasedIncomeAmount.sum

      val hasOccPension = if (occPensionAmt > 0) true else false
      val hasEmployment = if (ceasedAmt > 0 || empAmt > 0) true else false
      val totalEmploymentPensionAmt = empAmt + occPensionAmt + ceasedAmt

      (totalEmploymentPensionAmt, hasEmployment, hasOccPension)
    }.getOrElse(BigDecimal(0), false, false)


    val benefitsFromEmployment = increasesTax.map { incTax =>
      if (incTax.benefitsFromEmployment.isDefined)
        incTax.benefitsFromEmployment.get
      else
        TaxComponent(BigDecimal(0), 0, "", List.empty)
    }.getOrElse(TaxComponent(BigDecimal(0), 0, "", List.empty))

    val benefitsData = TaxSummaryHelper.createBenefitsTable(benefitsFromEmployment)
    val taxableBenefitsData = TaxSummaryHelper.createTaxableBenefitTable(nonTaxCodeIncomes, taxCodeIncomes)

    TaxableIncome(taxFreeAmount, incomeTax, income, taxCodeList, increasesTax, EmploymentPension(taxCodeIncomes,
      employmentPension._1, employmentPension._2, employmentPension._3), MessageWrapper.applyForList(investmentIncomeData._1), investmentIncomeData._2,
      MessageWrapper.applyForList(otherIncomeData._1), otherIncomeData._2, BenefitsDataWrapper.applyBenefit(benefitsData._1), benefitsData._2, MessageWrapper.applyForList(taxableBenefitsData._1), taxableBenefitsData._2,  TaxSummaryHelper.cyPlusOneAvailable(details))
  }
}


// TODO...REQUIRED FOR THE GATE KEEPER VIEW
object GateKeeperPageVM extends ViewModelFactory {
  override type ViewModelType = GateKeeperDetails
  override def createObject(nino:Nino, details: TaxSummaryDetails): GateKeeperDetails = {

    val decreasesTax = details.decreasesTax.getOrElse(new DecreasesTax(total = BigDecimal(0)))
    val liabilities = details.totalLiability.getOrElse(new TotalLiability(totalTax = BigDecimal(0), totalTaxOnIncome = BigDecimal(0)))
    val employments = details.taxCodeDetails.flatMap(_.employment)
    val employmentList = for ( emp <- employments.getOrElse(List())) yield (MessageWrapper(emp.name.getOrElse(""), emp.taxCode.getOrElse("")))
    val increasesTax = details.increasesTax.getOrElse(new IncreasesTax(total = BigDecimal(0)))
    new GateKeeperDetails(liabilities, decreasesTax, employmentList, increasesTax)
  }
}
