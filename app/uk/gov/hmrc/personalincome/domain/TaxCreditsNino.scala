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

import play.api.libs.json.{Reads, Writes}
import uk.gov.hmrc.domain.{SimpleName, SimpleObjectReads, SimpleObjectWrites, TaxIdentifier}

// Tax Credits Service allows certain Nino formats (e.g. NONOs) which are not permitted by standard uk.gov.hmrc.domain.Nino validation
case class TaxCreditsNino(taxCreditsNino: String) extends TaxIdentifier with SimpleName {
  require(TaxCreditsNino.isValid(taxCreditsNino), s"$taxCreditsNino is not a valid taxCreditsNino.")
  override lazy val toString = taxCreditsNino

  def value: String = taxCreditsNino

  val name = "taxCreditsNino"

  def formatted: String = value.grouped(2).mkString(" ")
}

object TaxCreditsNino {
  implicit val ninoWrite: Writes[TaxCreditsNino] = new SimpleObjectWrites[TaxCreditsNino](_.value)
  implicit val ninoRead: Reads[TaxCreditsNino] = new SimpleObjectReads[TaxCreditsNino]("taxCreditsNino", TaxCreditsNino.apply)

  private val validTaxCreditsNinoFormat = "^[A-Za-z]{2}( )?([0-9]{2} ?){3}[A-Da-d]{0,1}$"

  def isValid(taxCreditsNino: String): Boolean = taxCreditsNino != null && (taxCreditsNino.matches(validTaxCreditsNinoFormat))
}
