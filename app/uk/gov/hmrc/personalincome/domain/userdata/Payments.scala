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

package uk.gov.hmrc.personalincome.domain.userdata

import org.joda.time.DateTime
import play.api.libs.functional.FunctionalBuilder
import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.personalincome.domain.userdata.PaymentReadWriteUtils.{paymentReads, paymentWrites}

case class PaymentSummary(workingTaxCredit: Option[PaymentSection], childTaxCredit: Option[PaymentSection], paymentEnabled: Boolean, specialCircumstances: Option[String] = None) {

  def informationMessage: Option[String] = {
    if(specialCircumstances.isDefined) {
      Some(s"We are currently working out your payments as your child is changing their education or training. This should be done by 7 September ${DateTime.now.year.get}. If your child is staying in education or training, update their details on GOV.UK.")
    }
    else None
  }

  def totalsByDate: Option[List[Total]] = {
    total(workingTaxCredit.map(_.paymentSeq).getOrElse(Seq.empty)
      ++childTaxCredit.map(_.paymentSeq).getOrElse(Seq.empty))
  }

  def previousTotalsByDate: Option[List[Total]] = {
    total(workingTaxCredit.flatMap(_.previousPaymentSeq).getOrElse(Seq.empty)
        ++childTaxCredit.flatMap(_.previousPaymentSeq).getOrElse(Seq.empty))
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

case class PaymentSection(paymentSeq: List[FuturePayment], paymentFrequency: String,
                          previousPaymentSeq: Option[List[PastPayment]] = None)

trait Payment {
  val amount: BigDecimal
  val paymentDate: DateTime
  val oneOffPayment: Boolean
  val holidayType: Option[String]
  val earlyPayment: Boolean = holidayType.isDefined

  def oneOffPaymentText: String = ???
  def bankHolidayPaymentText: String = ???

  def explanatoryText: Option[String] = {
    if (oneOffPayment) Some(oneOffPaymentText)
    else if (earlyPayment) Some(bankHolidayPaymentText)
    else None
  }
}

case class FuturePayment(amount: BigDecimal, paymentDate: DateTime, oneOffPayment: Boolean, holidayType: Option[String] = None) extends Payment{
  override def oneOffPaymentText: String = "This is because of a recent change and is to help you get the right amount of tax credits."
  override def bankHolidayPaymentText: String = "Your payment is early because of UK bank holidays."
}

case class PastPayment(amount: BigDecimal, paymentDate: DateTime, oneOffPayment: Boolean, holidayType: Option[String] = None) extends Payment {
  override def oneOffPaymentText: String = "This was because of a recent change and was to help you get the right amount of tax credits."
  override def bankHolidayPaymentText: String = "Your payment was early because of UK bank holidays."
}

case class Total(amount: BigDecimal, paymentDate: DateTime)

object PaymentReadWriteUtils {
  val paymentReads: FunctionalBuilder[Reads]#CanBuild4[BigDecimal, DateTime, Boolean, Option[String]] =
    (JsPath \ "amount").read[BigDecimal] and
    (JsPath \ "paymentDate").read[DateTime] and
    (JsPath \ "oneOffPayment").read[Boolean] and
    (JsPath \ "holidayType").readNullable[String]

  val paymentWrites: OWrites[(BigDecimal, DateTime, Boolean, Option[String], Boolean, Option[String])] = (
    (__ \ "amount").write[BigDecimal] ~
      (__ \ "paymentDate").write[DateTime] ~
      (__ \ "oneOffPayment").write[Boolean] ~
      (__ \ "holidayType").writeNullable[String] ~
      (__ \ "earlyPayment").write[Boolean] ~
      (__ \ "explanatoryText").writeNullable[String]
    ).tupled
}

object FuturePayment {
  implicit val reads: Reads[FuturePayment] = paymentReads(FuturePayment.apply _)

  implicit val writes: Writes[FuturePayment] = new Writes[FuturePayment] {
    def writes(payment: FuturePayment) = {
      paymentWrites.writes((
        payment.amount, payment.paymentDate, payment.oneOffPayment, payment.holidayType, payment.earlyPayment, payment.explanatoryText))
    }
  }
}

object PastPayment {
  implicit val reads: Reads[PastPayment] = paymentReads(PastPayment.apply _)

  implicit val writes: Writes[PastPayment] = new Writes[PastPayment] {
    def writes(payment: PastPayment) = {
      paymentWrites.writes((
        payment.amount, payment.paymentDate, payment.oneOffPayment, payment.holidayType, payment.earlyPayment, payment.explanatoryText))
    }
  }
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
        (__ \ "totalsByDate").writeNullable[List[Total]] ~
        (__ \ "previousTotalsByDate").writeNullable[List[Total]]
      ).tupled

      paymentSummaryWrites.writes((paymentSummary.workingTaxCredit, paymentSummary.childTaxCredit, paymentSummary.paymentEnabled, paymentSummary.specialCircumstances, paymentSummary.informationMessage, paymentSummary.totalsByDate, paymentSummary.previousTotalsByDate))
    }
  }

}
