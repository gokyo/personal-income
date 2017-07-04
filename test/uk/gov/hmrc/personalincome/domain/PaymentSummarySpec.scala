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

import play.api.libs.json.{JsSuccess, Json}
import uk.gov.hmrc.personalincome.domain.userdata.PaymentSummary
import uk.gov.hmrc.play.test.UnitSpec

class PaymentSummarySpec extends UnitSpec {


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
            val request = """{"childTaxCredit": {
                            |    "paymentSeq": [
                            |      {
                            |        "amount": 55.00,
                            |        "paymentDate": 1499209200000,
                            |        "oneOffPayment": false
                            |      },
                            |      {
                            |        "amount": 55.00,
                            |        "paymentDate": 1499814000000,
                            |        "oneOffPayment": false
                            |      },
                            |      {
                            |        "amount": 55.00,
                            |        "paymentDate": 1500418800000,
                            |        "oneOffPayment": false
                            |      }
                            |    ],
                            |    "paymentFrequency": "weekly"
                            |  },
                            |  "paymentEnabled": true
                            |}""".stripMargin

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

      Json.stringify(Json.toJson(paymentSummary)) shouldBe """{"childTaxCredit":{"paymentSeq":[{"amount":55,"paymentDate":1499209200000,"oneOffPayment":false},{"amount":55,"paymentDate":1499814000000,"oneOffPayment":false},{"amount":55,"paymentDate":1500418800000,"oneOffPayment":false}],"paymentFrequency":"weekly"},"paymentEnabled":true,"totalsByDate":[{"amount":55,"paymentDate":1499209200000},{"amount":55,"paymentDate":1499814000000},{"amount":55,"paymentDate":1500418800000}]}"""
    }
    "parse correctly if no ctc is provided" in {
      val request = """{"workingTaxCredit": {
                      |    "paymentSeq": [
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1499209200000,
                      |        "oneOffPayment": false
                      |      },
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1499814000000,
                      |        "oneOffPayment": false
                      |      },
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1500418800000,
                      |        "oneOffPayment": false
                      |      }
                      |    ],
                      |    "paymentFrequency": "weekly"
                      |  },
                      |  "paymentEnabled": true
                      |}""".stripMargin

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

      Json.stringify(Json.toJson(paymentSummary)) shouldBe """{"workingTaxCredit":{"paymentSeq":[{"amount":55,"paymentDate":1499209200000,"oneOffPayment":false},{"amount":55,"paymentDate":1499814000000,"oneOffPayment":false},{"amount":55,"paymentDate":1500418800000,"oneOffPayment":false}],"paymentFrequency":"weekly"},"paymentEnabled":true,"totalsByDate":[{"amount":55,"paymentDate":1499209200000},{"amount":55,"paymentDate":1499814000000},{"amount":55,"paymentDate":1500418800000}]}"""
    }

    "parse correctly and sort calculated totalsByDate by Date" in {
      val request = """{
                      |  "workingTaxCredit": {
                      |    "paymentSeq": [
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1499814000000,
                      |        "oneOffPayment": false
                      |      },
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1500418800000,
                      |        "oneOffPayment": false
                      |      },
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1499209200000,
                      |        "oneOffPayment": false
                      |      }
                      |    ],
                      |    "paymentFrequency": "weekly"
                      |  },
                      |  "childTaxCredit": {
                      |    "paymentSeq": [
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1500418800000,
                      |        "oneOffPayment": false
                      |      },
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1499814000000,
                      |        "oneOffPayment": false
                      |      },
                      |      {
                      |        "amount": 55.00,
                      |        "paymentDate": 1499209200000,
                      |        "oneOffPayment": false
                      |      }
                      |    ],
                      |    "paymentFrequency": "weekly"
                      |  },
                      |  "paymentEnabled": true
                      |}""".stripMargin

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

      print( Json.stringify(Json.toJson(paymentSummary)))
      Json.stringify(Json.toJson(paymentSummary)) shouldBe """{"workingTaxCredit":{"paymentSeq":[{"amount":55,"paymentDate":1499814000000,"oneOffPayment":false},{"amount":55,"paymentDate":1500418800000,"oneOffPayment":false},{"amount":55,"paymentDate":1499209200000,"oneOffPayment":false}],"paymentFrequency":"weekly"},"childTaxCredit":{"paymentSeq":[{"amount":55,"paymentDate":1500418800000,"oneOffPayment":false},{"amount":55,"paymentDate":1499814000000,"oneOffPayment":false},{"amount":55,"paymentDate":1499209200000,"oneOffPayment":false}],"paymentFrequency":"weekly"},"paymentEnabled":true,"totalsByDate":[{"amount":110,"paymentDate":1499209200000},{"amount":110,"paymentDate":1499814000000},{"amount":110,"paymentDate":1500418800000}]}"""
    }

  }
}
