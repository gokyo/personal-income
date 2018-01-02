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

import java.text.DecimalFormat

/**
  * Parses formatted monetary amounts as output by personal-tax-summary into
  * BigDecimals.
  *
  * We do this so that we can send the amounts to the mobile apps in numeric
  * format so that the apps can decide how to format them.
  *
  * Ideally personal-tax-summary would not format the amounts and instead
  * return them as numeric values, leaving it to the UIs to format them (because
  * the UIs are likely to have better knowledge of the user's locale and
  * preferences). We hope to move to this design in future.
  *
  * NOT THREAD SAFE because each instance uses a DecimalFormat.
  */
class MoneyParser {

  private val format = new DecimalFormat("#,###.##")
  format.setParseBigDecimal(true)

  def parse(str: String): BigDecimal = BigDecimal(format.parse(str).asInstanceOf[java.math.BigDecimal])

}
