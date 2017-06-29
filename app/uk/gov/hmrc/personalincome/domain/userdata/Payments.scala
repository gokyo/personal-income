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

import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.joda.time.{DateTime, LocalDate}
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsError, _}
import play.api.libs.functional.syntax._

import scala.util.matching.Regex

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
  def totalsByDate: List[Total] = {
    val wtc = workingTaxCredit.map(_.paymentSeq).getOrElse(List())
    val ctc = childTaxCredit.map(_.paymentSeq).getOrElse(List())
    val all: Seq[Payment] = wtc.union(ctc)
    all.map(_.paymentDate).distinct.map(date => Total(all.filter(_.paymentDate.equals(date)).foldLeft(0.0)(_ + _.amount),date)).toList
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
    def writes(paymentSummary: PaymentSummary): JsObject = {
      Json.obj(
        "workingTaxCredit" -> paymentSummary.workingTaxCredit,
        "childTaxCredit" -> paymentSummary.childTaxCredit,
        "paymentEnabled" -> paymentSummary.paymentEnabled,
        "totalsByDate" -> Json.toJson(paymentSummary.totalsByDate)
      )
    }
  }
}

