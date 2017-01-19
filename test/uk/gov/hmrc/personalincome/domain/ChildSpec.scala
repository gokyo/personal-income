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

import uk.gov.hmrc.personalincome.domain.userdata.Child
import uk.gov.hmrc.play.test.UnitSpec
import uk.gov.hmrc.time.DateTimeUtils

class ChildSpec extends UnitSpec {

  "Child DOB calculation" should {

    "using today's date for child DOB will result in age 0" in {
      val childA = Child("first", "second", DateTimeUtils.now, false, false, false)

      Child.getAge(childA) shouldBe 0
    }

    "using today's date - 16 years for child DOB will result in age 16" in {
      val childB = Child("first", "second", DateTimeUtils.now.minusYears(16), false, false, false)

      Child.getAge(childB) shouldBe 16
    }

    "using today's date - 16 years and +1 month for child DOB will result in age 15" in {
      val age= DateTimeUtils.now.minusYears(15)

      val childC = Child("first", "second", DateTimeUtils.now.minusYears(16).plusMonths(1), false, false, false)

      Child.getAge(childC) shouldBe 15
    }

  }
}
