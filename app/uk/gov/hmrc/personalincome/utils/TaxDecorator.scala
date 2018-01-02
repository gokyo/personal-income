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


case class TaxDecorator (tax:Tax, employmentCount:Int=1, maxDisplayValueIsIncome:Boolean=false, paAmount: BigDecimal=BigDecimal(0)){

  val taxBandDecorator = new TaxBandDecorator(tax, maxDisplayValueIsIncome, paAmount)

  lazy val nextBandDescription :Option[TaxBandDescription] = taxBandDecorator.nextBandDescription
  lazy val taxBandDescriptions :List[TaxBandDescription] =
    taxBandDecorator.taxBandDescriptions(taxFreeMessage=taxBreakdownTaxFreeMessage, taxBandMessageFunc=taxBandKeyDescription)
  lazy val taxFreeAllocationDescriptions :List[TaxBandDescription] =
    taxBandDecorator.taxBandDescriptions(taxFreeMessage=taxFreeAllocationMessage, taxBandMessageFunc=totalLiabilityTaxBandMessage)

  lazy val incomeAsPercentage :BigDecimal = taxBandDecorator.incomeAsPercentage
  lazy val totalIncome :BigDecimal = taxBandDecorator.totalIncome

  lazy val taxBandLabelFirstAmount = taxBandDecorator.taxBandLabelFirstAmount
  lazy val taxBandLabelMiddle = taxBandDecorator.taxBandLabelMiddle
  lazy val taxBandLabelLastAmount = taxBandDecorator.taxBandLabelLastAmount


   lazy val taxBreakdownTaxFreeMessage : Option[String] = {
    if (!taxBandDecorator.hasIncome) {
      None
    }else if (taxBandDecorator.isAllTaxFree) {
      //Some(Messages("tai.taxCalc.taxFreeAmount.all", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
      Some(Messages("tai.taxCalcIncome.taxFreeAmount.available", BigDecimal(0),   MoneyPounds(taxBandDecorator.allowReliefDeducts, 0).quantity))

    }else if (taxBandDecorator.hasTaxFreeAmount) {
      Some(Messages("tai.taxCalc.taxFreeAmount.available", MoneyPounds(BigDecimal(0), 0).quantity,
        MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
    }else if(taxBandDecorator.hasKCode) {
      Some(Messages("tai.taxCalc.taxFreeAmount.extraBurden", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
    }else {
      None
    }
  }

  lazy val taxFreeExtraIncomeMessage : Option[String] = {
    val nonZeroBands = taxBandDecorator.adjustedTaxBands.getOrElse(Nil).filter(_.rate == Some(BigDecimal(0)))
    nonZeroBands match {
      case x if (x.size > 0 && taxBandDecorator.isAllTaxFree) => {
        val rate = nonZeroBands(0).rate.getOrElse(BigDecimal(0))
        val startOfBasicRate = nonZeroBands(0).lowerBand.getOrElse(BigDecimal(0))
        Some(Messages("tai.taxCalc.", rate, startOfBasicRate))
      }
      case _ => None
    }
  }


   def taxBandKeyDescription(band : TaxBandDescription, onlyMessage:Boolean, firstMessage:Boolean, lastMessage:Boolean):String = {
    if (onlyMessage) {
      Messages("tai.taxCalcKey.bands.all", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (firstMessage) {
      Messages("tai.taxCalcKey.bands.first", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (lastMessage) {
      Messages("tai.taxCalcKey.bands.lastBand", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else {
      Messages("tai.taxCalcKey.bands.range", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity,
        MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }
  }


  private lazy val taxFreeAllocationMessage : Option[String] = {
    if (!taxBandDecorator.hasIncome) {
      None
    }else if (taxBandDecorator.hasTaxFreeAmount) {
      if (employmentCount > 1) {
        Some(Messages("tai.taxCalc.taxFreeAllocation.multiIncome", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
      }else {
        Some(Messages("tai.taxCalc.taxFreeAllocation.singleIncome", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
      }
    } else if(taxBandDecorator.hasKCode) {
      Some(Messages("tai.taxCalc.taxFreeAllocation.extraBurden", MoneyPounds(taxBandDecorator.taxFreeAmount, 0).quantity))
    }else {
      None
    }
  }

  def totalLiabilityTaxBandMessage(band : TaxBandDescription, onlyMessage:Boolean, firstMessage:Boolean, lastMessage:Boolean):String = {
    if (onlyMessage) {
      Messages("tai.taxCalc.bands.all", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (firstMessage) {
      Messages("tai.taxCalc.bands.first", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else if (lastMessage) {
      Messages("tai.taxCalc.bands.lastBand", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }else {
      Messages("tai.taxCalc.bands.range", band.taxBand.rate.getOrElse(BigDecimal(0)),
        MoneyPounds(band.taxBand.income.getOrElse(BigDecimal(0)), 0).quantity, MoneyPounds(band.taxBand.tax.getOrElse(BigDecimal(0)), 0).quantity)
    }
  }
}
