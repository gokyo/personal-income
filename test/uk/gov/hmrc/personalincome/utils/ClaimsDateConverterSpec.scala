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

import uk.gov.hmrc.play.test.UnitSpec

class ClaimsDateConverterSpec extends UnitSpec{
  private val dateConverter = new ClaimsDateConverter

  "convert date format" should {
    "convert a string formatted yyyy-MM-dd to dd/MM/yyyy" in {
      dateConverter.convertDateFormat("2018-01-02") shouldBe Some("02/01/2018")
    }

    "convert a string formatted yyyyMMdd to dd/MM/yyyy" in {
      dateConverter.convertDateFormat("20180102") shouldBe Some("02/01/2018")
    }

    "not convert a string with an invalid date format" in {
      dateConverter.convertDateFormat("20180102_invalid") shouldBe None
    }

    "not convert an empty string " in {
      dateConverter.convertDateFormat("") shouldBe None
    }
  }
}
