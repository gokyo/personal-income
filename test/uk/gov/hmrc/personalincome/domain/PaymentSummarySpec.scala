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

import org.joda.time.DateTime
import play.api.libs.json.{JsSuccess, Json}
import uk.gov.hmrc.personalincome.domain.userdata.{FuturePayment, PastPayment, PaymentSummary}
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class PaymentSummarySpec extends UnitSpec with WithFakeApplication {

  val now = DateTime.now
  def date(n: Int, func:(Int) ⇒ DateTime): Long = func(n).getMillis

  def payment(amount: Double, paymentDate: Long, oneOffPayment: Boolean, holidayType: Option[String] = None, explanatoryText: Option[String] = None): String = {
    val bankHolidayJson = if (holidayType.isDefined) s"""| "holidayType": "${holidayType.get}", """ else ""
    val explanatoryTextJson = if (explanatoryText.isDefined) s"""|, "explanatoryText": "${explanatoryText.get}" """ else ""

    s"""{
       | "amount": $amount,
       | "paymentDate": $paymentDate,
       | "oneOffPayment": $oneOffPayment,
       $bankHolidayJson
       | "earlyPayment": ${holidayType.isDefined}
       $explanatoryTextJson
       |}""".stripMargin
  }

  def total(amount: Double, paymentDate: Long): String = {
    s"""{
       |"amount": $amount,
       |"paymentDate": $paymentDate
       |}""".stripMargin
  }

  private val futureEarlyPaymentText = Some("Your payment is early because of UK bank holidays.")
  private val pastEarlyPaymentText = Some("Your payment was early because of UK bank holidays.")
  private val futureOneOffPaymentText = Some("This is because of a recent change and is to help you get the right amount of tax credits.")
  private val pastOneOffPaymentText = Some("This was because of a recent change and was to help you get the right amount of tax credits.")

  private val bankHoliday = Some("bankHolidsy")

  "PaymentSummary" should {
    "parse correctly if no wtc or ctc is provided" in {
      val request =
        """{"paymentEnabled":true}""".stripMargin

      val response = Json.parse(request).validate[PaymentSummary]
      val paymentSummary = response match {
          case success: JsSuccess[PaymentSummary] => {
            success.get
          }
      }
      paymentSummary.paymentEnabled shouldBe true
      paymentSummary.childTaxCredit shouldBe None
      paymentSummary.workingTaxCredit shouldBe None
      paymentSummary.totalsByDate.isDefined shouldBe false

      Json.stringify(Json.toJson(paymentSummary)) shouldBe request
    }
    "parse correctly if no wtc is provided" in {
      val ctc =
        s"""
           |"childTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly"
           |}
         """.stripMargin

      val totalsByDate =
        s"""
           |"totalsByDate": [
           |  ${total(55.00, date(1, now.plusMonths))},
           |  ${total(55.00, date(2, now.plusMonths))},
           |  ${total(55.00, date(3, now.plusMonths))}
           |]
         """.stripMargin

      val request = s"""{$ctc, "paymentEnabled": true}""".stripMargin
      val expectedResponse = Json.stringify(Json.parse(s"""{ $ctc, "paymentEnabled": true, $totalsByDate }"""))
      val response = Json.parse(request).validate[PaymentSummary]
      val paymentSummary = response match {
          case success: JsSuccess[PaymentSummary] => {
            success.get
          }
      }
      paymentSummary.paymentEnabled shouldBe true
      paymentSummary.childTaxCredit.isDefined shouldBe true
      paymentSummary.workingTaxCredit.isDefined shouldBe false
      paymentSummary.totalsByDate.isDefined shouldBe true

      Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
    }
    "parse correctly if no ctc is provided" in {

      val wtc =
        s"""
           |"workingTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly"
           |}
         """.stripMargin

      val totalsByDate =
        s"""
           |"totalsByDate": [
           |  ${total(55.00, date(1, now.plusMonths))},
           |  ${total(55.00, date(2, now.plusMonths))},
           |  ${total(55.00, date(3, now.plusMonths))}
           |]
         """.stripMargin

      val request = s"""{$wtc,"paymentEnabled": true}""".stripMargin
      val expectedResponse = Json.stringify(Json.parse(s"""{ $wtc, "paymentEnabled": true, $totalsByDate }"""))
      val response = Json.parse(request).validate[PaymentSummary]
      val paymentSummary = response match {
        case success: JsSuccess[PaymentSummary] => {
          success.get
        }
      }
      paymentSummary.paymentEnabled shouldBe true
      paymentSummary.childTaxCredit.isDefined shouldBe false
      paymentSummary.workingTaxCredit.isDefined shouldBe true
      paymentSummary.totalsByDate.isEmpty shouldBe false

      Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
    }
    "parse correctly and sort calculated totalsByDate by Date" in {

      val wtc =
        s"""
           |"workingTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly"
           |}
         """.stripMargin

      val ctc =
        s"""
           |"childTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly"
           |}
         """.stripMargin

      val totalsByDate =
        s"""
           |"totalsByDate": [
           |  ${total(110.00, date(1, now.plusMonths))},
           |  ${total(110.00, date(2, now.plusMonths))},
           |  ${total(110.00, date(3, now.plusMonths))}
           |]
         """.stripMargin

      val request = s"""{ $wtc, $ctc, "paymentEnabled": true}""".stripMargin
      val expectedResponse = Json.stringify(Json.parse(s"""{ $wtc, $ctc, "paymentEnabled": true, $totalsByDate }"""))

      val response = Json.parse(request).validate[PaymentSummary]
      val paymentSummary = response match {
        case success: JsSuccess[PaymentSummary] => {
          success.get
        }
      }
      paymentSummary.paymentEnabled shouldBe true
      paymentSummary.childTaxCredit.isDefined shouldBe true
      paymentSummary.workingTaxCredit.isDefined shouldBe true
      paymentSummary.totalsByDate.isDefined shouldBe true

      Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
    }
    "correctly parse the previous payments and associated totals for wtc" in {

      val wtc =
        s"""
           |"workingTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly",
           |  "previousPaymentSeq": [
           |    ${payment(33.00, date(1, now.minusMonths), false)},
           |    ${payment(43.00, date(2, now.minusMonths), true, None, pastOneOffPaymentText)},
           |    ${payment(53.00, date(3, now.minusMonths), false, bankHoliday, pastEarlyPaymentText)}
           |  ]
           |}
         """.stripMargin

      val ctc =
        s"""
           |"childTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly"
           |}
         """.stripMargin

      val totalsByDate =
        s"""
           |"totalsByDate": [
           |  ${total(110.00, date(1, now.plusMonths))},
           |  ${total(110.00, date(2, now.plusMonths))},
           |  ${total(110.00, date(3, now.plusMonths))}
           |]
         """.stripMargin

      val previousTotalsByDate =
        s"""
           |"previousTotalsByDate": [
           | ${total(53.00, date(3, now.minusMonths))},
           | ${total(43.00, date(2, now.minusMonths))},
           | ${total(33.00, date(1, now.minusMonths))}
           |]
         """.stripMargin

      val request = s"""{ $wtc, $ctc, "paymentEnabled": true}""".stripMargin
      val expectedResponse = Json.stringify(Json.parse(s"""{ $wtc, $ctc, "paymentEnabled": true, $totalsByDate, $previousTotalsByDate }"""))
      val response = Json.parse(request).validate[PaymentSummary]
      val paymentSummary = response match {
        case success: JsSuccess[PaymentSummary] => {
          success.get
        }
      }
      paymentSummary.paymentEnabled shouldBe true
      paymentSummary.childTaxCredit.isDefined shouldBe true
      paymentSummary.workingTaxCredit.isDefined shouldBe true
      paymentSummary.totalsByDate.isDefined shouldBe true
      paymentSummary.previousTotalsByDate.isDefined shouldBe true
      Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
    }
    "correctly parse the previous payments and associated totals for ctc" in {

      val wtc =
        s"""
           |"workingTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly"
           |}
         """.stripMargin

      val ctc =
        s"""
           |"childTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(55.00, date(1, now.plusMonths), false)},
           |    ${payment(55.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(55.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly",
           |  "previousPaymentSeq": [
           |    ${payment(33.00, date(1, now.minusMonths), false)},
           |    ${payment(43.00, date(2, now.minusMonths), true, None, pastOneOffPaymentText)},
           |    ${payment(53.00, date(3, now.minusMonths), false, bankHoliday, pastEarlyPaymentText)}
           |  ]
           |}
         """.stripMargin

      val totalsByDate =
        s"""
           |"totalsByDate": [
           |  ${total(110.00, date(1, now.plusMonths))},
           |  ${total(110.00, date(2, now.plusMonths))},
           |  ${total(110.00, date(3, now.plusMonths))}
           |]
         """.stripMargin

      val previousTotalsByDate =
        s"""
           |"previousTotalsByDate": [
           | ${total(53.00, date(3, now.minusMonths))},
           | ${total(43.00, date(2, now.minusMonths))},
           | ${total(33.00, date(1, now.minusMonths))}
           |]
         """.stripMargin

      val request = s"""{ $wtc, $ctc, "paymentEnabled": true}""".stripMargin
      val expectedResponse = Json.stringify(Json.parse(s"""{ $wtc, $ctc, "paymentEnabled": true, $totalsByDate, $previousTotalsByDate }"""))
      val response = Json.parse(request).validate[PaymentSummary]
      val paymentSummary = response match {
        case success: JsSuccess[PaymentSummary] => {
          success.get
        }
      }
      paymentSummary.paymentEnabled shouldBe true
      paymentSummary.childTaxCredit.isDefined shouldBe true
      paymentSummary.workingTaxCredit.isDefined shouldBe true
      paymentSummary.totalsByDate.isDefined shouldBe true
      paymentSummary.previousTotalsByDate.isDefined shouldBe true
      Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
    }
    "totals are calculated correctly for wtc and ctc with future and previous payments" in {
      val wtc =
        s"""
           |"workingTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(21.33, date(1, now.plusMonths), false)},
           |    ${payment(33.33, date(2, now.plusMonths), false)},
           |    ${payment(33.33, date(2, now.plusMonths), false)},
           |    ${payment(22.95, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(89.61, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly",
           |  "previousPaymentSeq": [
           |    ${payment(33.12, date(2, now.minusMonths), false)},
           |    ${payment(33.56, date(2, now.minusMonths), false)},
           |    ${payment(53.65, date(5, now.minusMonths), false)},
           |    ${payment(50.35, date(5, now.minusMonths), true, None, pastOneOffPaymentText)},
           |    ${payment(53.00, date(5, now.minusMonths), false, bankHoliday, pastEarlyPaymentText)}
           |  ]
           |}
         """.stripMargin

      val ctc =
        s"""
           |"childTaxCredit": {
           |  "paymentSeq": [
           |    ${payment(105.88, date(1, now.plusMonths), false)},
           |    ${payment(100.55, date(2, now.plusMonths), false)},
           |    ${payment(5.33, date(2, now.plusMonths), false)},
           |    ${payment(100.55, date(3, now.plusMonths), false)},
           |    ${payment(2.66, date(3, now.plusMonths), true, None, futureOneOffPaymentText)},
           |    ${payment(2.67, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
           |  ],
           |  "paymentFrequency": "weekly",
           |  "previousPaymentSeq": [
           |    ${payment(333.33, date(1, now.minusMonths), false)},
           |    ${payment(333.33, date(1, now.minusMonths), false)},
           |    ${payment(333.33, date(1, now.minusMonths), false)},
           |    ${payment(213.00, date(2, now.minusMonths), false)},
           |    ${payment(213.00, date(2, now.minusMonths), false)},
           |    ${payment(213.00, date(2, now.minusMonths), false)},
           |    ${payment(360.99, date(2, now.minusMonths), false)},
           |    ${payment(153.12, date(3, now.minusMonths), true, None, pastOneOffPaymentText)},
           |    ${payment(846.87, date(3, now.minusMonths), false, bankHoliday, pastEarlyPaymentText)}
           |  ]
           |}
         """.stripMargin

      val totalsByDate =
        s"""
           |"totalsByDate": [
           |  ${total(127.21, date(1, now.plusMonths))},
           |  ${total(195.49, date(2, now.plusMonths))},
           |  ${total(195.49, date(3, now.plusMonths))}
           |]
         """.stripMargin

      val previousTotalsByDate =
        s"""
           |"previousTotalsByDate": [
           | ${total(157.00, date(5, now.minusMonths))},
           | ${total(999.99, date(3, now.minusMonths))},
           | ${total(1066.67, date(2, now.minusMonths))},
           | ${total(999.99, date(1, now.minusMonths))}
           |]
         """.stripMargin

      val request = s"""{ $wtc, $ctc, "paymentEnabled": true}""".stripMargin
      val expectedResponse = Json.stringify(Json.parse(s"""{ $wtc, $ctc, "paymentEnabled": true, $totalsByDate, $previousTotalsByDate }"""))
      val response = Json.parse(request).validate[PaymentSummary]
      val paymentSummary = response match {
        case success: JsSuccess[PaymentSummary] => {
          success.get
        }
      }
      paymentSummary.paymentEnabled shouldBe true
      paymentSummary.childTaxCredit.isDefined shouldBe true
      paymentSummary.workingTaxCredit.isDefined shouldBe true
      paymentSummary.totalsByDate.isDefined shouldBe true
      paymentSummary.previousTotalsByDate.isDefined shouldBe true
      Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
    }
  }
  "correctly parse the informationMessage" in {
    val wtc =
      s"""
         |"workingTaxCredit": {
         |  "paymentSeq": [
         |    ${payment(50.00, date(1, now.plusMonths), false)},
         |    ${payment(82.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
         |    ${payment(82.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
         |  ],
         |  "paymentFrequency": "weekly"
         |}
         """.stripMargin
    val ctc =
      s"""
         |"childTaxCredit": {
         |  "paymentSeq": [
         |    ${payment(25.00, date(1, now.plusMonths), false)},
         |    ${payment(25.00, date(2, now.plusMonths), true, None, futureOneOffPaymentText)},
         |    ${payment(50.00, date(3, now.plusMonths), false, bankHoliday, futureEarlyPaymentText)}
         |  ],
         |  "paymentFrequency": "weekly"
         |}
         """.stripMargin
    val totalsByDate =
      s"""
         |"totalsByDate": [
         |  ${total(75.00, date(1, now.plusMonths))},
         |  ${total(107.00, date(2, now.plusMonths))},
         |  ${total(132.00, date(3, now.plusMonths))}
         |]
         """.stripMargin

    val request = s"""{$wtc, $ctc, "specialCircumstances": "FTNAE", "paymentEnabled": true}""".stripMargin
    val expectedResponse = Json.stringify(Json.parse(
      s"""{
         |$wtc, $ctc,
         |"paymentEnabled": true,
         |"specialCircumstances":"FTNAE",
         |"informationMessage": "We are currently working out your payments as your child is changing their education or training. This should be done by 7 September 2017. If your child is staying in education or training, update their details on GOV.UK.",
         |$totalsByDate
         |}""".stripMargin))
    val response = Json.parse(request).validate[PaymentSummary]
    val paymentSummary = response match {
      case success: JsSuccess[PaymentSummary] => {
        success.get
      }
    }
    paymentSummary.paymentEnabled shouldBe true
    paymentSummary.childTaxCredit.isDefined shouldBe true
    paymentSummary.workingTaxCredit.isDefined shouldBe true
    paymentSummary.informationMessage.isDefined shouldBe true
    paymentSummary.totalsByDate.isDefined shouldBe true

    Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
  }
  "correctly parse NO informationMessage if specialCircumstance value is not configured" in {
    val wtc =
      s"""
         |"workingTaxCredit": {
         |  "paymentSeq": [
         |    ${payment(50.00, date(1, now.plusMonths), false)}
         |  ],
         |  "paymentFrequency": "weekly"
         |}
         """.stripMargin
    val ctc =
      s"""
         |"childTaxCredit": {
         |  "paymentSeq": [
         |    ${payment(25.00, date(2, now.plusMonths), false)}
         |  ],
         |  "paymentFrequency": "weekly"
         |}
         """.stripMargin
    val totalsByDate =
      s"""
         |"totalsByDate": [
         |  ${total(50.00, date(1, now.plusMonths))},
         |  ${total(25.00, date(2, now.plusMonths))}
         |]
         """.stripMargin
    val request = s"""{$wtc,$ctc,
                    |  "specialCircumstances": "This key has not been configured",
                    |  "paymentEnabled": true
                    |}""".stripMargin
    val expectedResponse = Json.stringify(Json.parse(
      s"""
         |{$wtc,$ctc,
         |"paymentEnabled": true,
         |"specialCircumstances":"This key has not been configured",
         |$totalsByDate
         |}""".stripMargin))
    val response = Json.parse(request).validate[PaymentSummary]
    val paymentSummary = response match {
      case success: JsSuccess[PaymentSummary] => {
        success.get
      }
    }
    paymentSummary.paymentEnabled shouldBe true
    paymentSummary.childTaxCredit.isDefined shouldBe true
    paymentSummary.workingTaxCredit.isDefined shouldBe true
    paymentSummary.informationMessage.isDefined shouldBe false
    paymentSummary.totalsByDate.isDefined shouldBe true

    Json.stringify(Json.toJson(paymentSummary)) shouldBe expectedResponse
  }

  "Future Payment " should {
    "return the correct explanatory text for a one-off payment" in {
      FuturePayment(1, now, oneOffPayment = false).explanatoryText shouldBe None
      FuturePayment(1, now, oneOffPayment = true).explanatoryText shouldBe
        Some("This is because of a recent change and is to help you get the right amount of tax credits.")
    }

    "return the correct explanatory text for a bank holiday payment" in {
      FuturePayment(1, now, oneOffPayment = false, holidayType = Some("bankHoliday")).explanatoryText shouldBe
        futureEarlyPaymentText
    }

    "return the one-off payment explanatory text for a one-off payment made early due to a bank holiday" in {
      FuturePayment(1, now, oneOffPayment = true, holidayType = Some("bankHoliday")).explanatoryText shouldBe
        Some("This is because of a recent change and is to help you get the right amount of tax credits.")
    }
  }

  "Past Payment " should {
    "return the correct explanatory text for a one-off payment" in {
      PastPayment(1, now, oneOffPayment = false).explanatoryText shouldBe None
      PastPayment(1, now, oneOffPayment = true).explanatoryText shouldBe
        Some("This was because of a recent change and was to help you get the right amount of tax credits.")
    }

    "return the correct explanatory text for a bank holiday payment" in {
      PastPayment(1, now, oneOffPayment = false, holidayType = Some("bankHoliday")).explanatoryText shouldBe
        Some("Your payment was early because of UK bank holidays.")
    }

    "return the one-off payment explanatory text for a one-off payment made early due to a bank holiday" in {
      PastPayment(1, now, oneOffPayment = true, holidayType = Some("bankHoliday")).explanatoryText shouldBe
        Some("This was because of a recent change and was to help you get the right amount of tax credits.")
    }
  }
}
