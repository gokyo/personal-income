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

package uk.gov.hmrc.personalincome.domain.userdata

import org.joda.time.DateTime
import play.api.Play
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class PaymentSummary(workingTaxCredit: Option[PaymentSection], childTaxCredit: Option[PaymentSection], paymentEnabled: Boolean, specialCircumstances: Option[String] = None) {

  def informationMessage: Option[String] = {
    if(specialCircumstances.isDefined) {
      Play.current.configuration.getString(s"specialCircumstanceMessage.${specialCircumstances.get}")
    }
    else None
  }

  def totalsByDate: Option[List[Total]] = {
    total(workingTaxCredit.map(_.paymentSeq).getOrElse(Seq.empty)
      ++childTaxCredit.map(_.paymentSeq).getOrElse(Seq.empty))
  }

  private def total(payments: Seq[Payment]): Option[List[Total]] = {
    if(payments.isEmpty) None
    else {
      val distinctDate  = payments.map(_.paymentDate).distinct.sortBy(_.toDate)
      Option(distinctDate.map(date => Total(payments.filter(_.paymentDate.equals(date))
        .foldLeft(BigDecimal(0))(_ + _.amount),date)).toList)
    }
  }
}

case class PaymentSection(paymentSeq: List[Payment], paymentFrequency: String)

case class Payment(amount: BigDecimal, paymentDate: DateTime, oneOffPayment: Boolean)

case class Total(amount: BigDecimal, paymentDate: DateTime)

object Payment {
  implicit val formats = Json.format[Payment]
}

object PaymentSection {
  implicit val formats = Json.format[PaymentSection]
}

object Total {
  implicit val formats = Json.format[Total]
}

object PaymentSummary {

  def key: String = "payment-data"

  implicit val reads: Reads[PaymentSummary] = (
      (JsPath \ "workingTaxCredit").readNullable[PaymentSection] and
      (JsPath \ "childTaxCredit").readNullable[PaymentSection] and
      (JsPath \ "paymentEnabled").read[Boolean] and
      (JsPath \ "specialCircumstances").readNullable[String]
    )(PaymentSummary.apply _)

  implicit val writes: Writes[PaymentSummary] = new Writes[PaymentSummary] {

    def writes(paymentSummary: PaymentSummary) = {
      val paymentSummaryWrites = (
        (__ \ "workingTaxCredit").writeNullable[PaymentSection] ~
        (__ \ "childTaxCredit").writeNullable[PaymentSection] ~
        (__ \ "paymentEnabled").write[Boolean] ~
        (__ \ "specialCircumstances").writeNullable[String] ~
        (__ \ "informationMessage").writeNullable[String] ~
        (__ \ "totalsByDate").writeNullable[List[Total]]
      ).tupled

      paymentSummaryWrites.writes(paymentSummary.workingTaxCredit, paymentSummary.childTaxCredit, paymentSummary.paymentEnabled, paymentSummary.specialCircumstances, paymentSummary.informationMessage, paymentSummary.totalsByDate)
    }
  }


}

