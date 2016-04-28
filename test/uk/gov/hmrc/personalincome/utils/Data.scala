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

package uk.gov.hmrc.personalincome.utils

import java.io.File

import play.api.libs.json.Json
import uk.gov.hmrc.personalincome.domain._

import scala.io.BufferedSource

object Data {

  private lazy val everything = "Everything/TaxSummary.json"
  private lazy val baseLevelView = "Everything/BaseLevelView.json"
  private lazy val estimatedIncome = "Everything/EstimatedIncome.json"
  private lazy val taxableIncome = "Everything/TaxableIncome.json"
  private lazy val taxSummaryContainer = "Everything/TaxSummaryContainer.json"

  private val basePath = "test/uk/gov/hmrc/personalincome/data/"

  private def getResource(fileName: String) = {
    val jsonFilePath = basePath + fileName
    val file : File = new File(jsonFilePath)

    val source:BufferedSource = scala.io.Source.fromFile(file)
    Json.parse(source.mkString(""))
  }

  private def getBaseViewModelR(fileName: String):BaseViewModel = {
    val result = Json.fromJson[BaseViewModel](getResource(fileName))
    result.getOrElse(throw new IllegalArgumentException("Failed to load resource!"))
  }

  private def getTaxSummaryR(fileName: String):TaxSummaryDetails = {
    val result = Json.fromJson[TaxSummaryDetails](getResource(fileName))
    result.getOrElse(throw new IllegalArgumentException("Failed to load resource!"))
  }

  private def getEstimatedIncomeR(fileName: String):EstimatedIncome = {
    val result = Json.fromJson[EstimatedIncome](getResource(fileName))
    result.getOrElse(throw new IllegalArgumentException("Failed to load resource!"))
  }

  private def getTaxableIncomeR(fileName: String):TaxableIncome = {
    val result = Json.fromJson[TaxableIncome](getResource(fileName))
    result.getOrElse(throw new IllegalArgumentException("Failed to load resource!"))
  }

  private def getTaxablSummaryContainerR(fileName: String):TaxSummaryContainer = {
    val result = Json.fromJson[TaxSummaryContainer](getResource(fileName))
    result.getOrElse(throw new IllegalArgumentException("Failed to load resource!"))
  }

  def getEverything = getTaxSummaryR(everything)
  def getBaseLevelView = getBaseViewModelR(baseLevelView)
  def getEstimatedIncome = getEstimatedIncomeR(estimatedIncome)
  def getTaxableIncome = getTaxableIncomeR(taxableIncome)
  def getTaxSummaryContainer = getTaxablSummaryContainerR(taxSummaryContainer)
}
