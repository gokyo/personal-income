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
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class PaymentSummaryOld(workingTaxCredit: Option[PaymentOld], childTaxCredit: Option[PaymentOld])

case class PaymentOld(amount: Double, paymentDate: DateTime, paymentFrequency:Option[String])

object PaymentOld {
  implicit val formats = Json.format[PaymentOld]
}

object PaymentSummaryOld {
  def key: String = "payments-data"

  implicit val formats = Json.format[PaymentSummaryOld]
}

case class PaymentSummary(workingTaxCredit: Option[PaymentSection], childTaxCredit: Option[PaymentSection], paymentEnabled: Boolean) {
  def totalsByDate: Option[List[Total]] = {
    val wtc = workingTaxCredit.map(_.paymentSeq).getOrElse(List())
    val ctc = childTaxCredit.map(_.paymentSeq).getOrElse(List())
    val all: Seq[Payment] = wtc.union(ctc)
    if (all.isEmpty) None
    else {
      val distinctDate  = all.map(_.paymentDate).distinct.sortBy(_.toDate)
      Option(distinctDate.map(date => Total(all.filter(_.paymentDate.equals(date)).foldLeft(0.0)(_ + _.amount),date)).toList)
    }
  }
}

case class PaymentSection(paymentSeq: List[Payment], paymentFrequency: String)

case class Payment(amount: Double, paymentDate: DateTime, oneOffPayment: Boolean)

case class Total(amount: Double, paymentDate: DateTime)

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
      (JsPath \ "paymentEnabled").read[Boolean]
    )(PaymentSummary.apply _)

  implicit val writes: Writes[PaymentSummary] = new Writes[PaymentSummary] {

    def writes(paymentSummary: PaymentSummary) = {
      val paymentSummaryWrites = (
        (__ \ "workingTaxCredit").writeNullable[PaymentSection] ~
          (__ \ "childTaxCredit").writeNullable[PaymentSection] ~
          (__ \ "paymentEnabled").write[Boolean] ~
          (__ \ "totalsByDate").writeNullable[List[Total]]
      ).tupled

      paymentSummaryWrites.writes(paymentSummary.workingTaxCredit, paymentSummary.childTaxCredit, paymentSummary.paymentEnabled, paymentSummary.totalsByDate)
    }
  }


}

