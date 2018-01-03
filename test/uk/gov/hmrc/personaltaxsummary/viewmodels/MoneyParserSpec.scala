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

package uk.gov.hmrc.personaltaxsummary.viewmodels

import org.scalatest.{Matchers, WordSpec}

class MoneyParserSpec extends WordSpec with Matchers {
  "parse" should {
    "parse numbers containing thousand separators into a BigDecimal" in {
      val parser = new MoneyParser

      parser.parse("12,345,678.91") shouldBe BigDecimal("12345678.91")
    }

    // in case the format returned by personal-tax-summary changes
    "parse numbers without thousand separators into a BigDecimal" in {
      val parser = new MoneyParser

      parser.parse("12345678.91") shouldBe BigDecimal("12345678.91")
    }
  }
}
