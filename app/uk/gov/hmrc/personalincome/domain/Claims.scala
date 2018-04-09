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

import play.api.libs.json.{Format, Json}


case class Claim(household: Household, renewal: Renewal)

object Claim {
  implicit val formats: Format[Claim] = Json.format[Claim]
}

case class Claims(references: Option[Seq[Claim]])

object Claims {
  implicit val formats: Format[Claims] = Json.format[Claims]
}


case class Applicant(nino: String,
                     title: String,
                     firstForename: String,
                     secondForename: Option[String],
                     surname: String)

object Applicant {
  implicit val formats: Format[Applicant] = Json.format[Applicant]
}

case class Household(barcodeReference: String,
                     applicationID: String,
                     applicant1: Applicant,
                     applicant2: Option[Applicant],
                     householdCeasedDate: Option[String],
                     householdEndReason: Option[String])

object Household {
  implicit val formats: Format[Household] = Json.format[Household]
}

case class Renewal(awardStartDate: Option[String],
                   awardEndDate: Option[String],
                   renewalStatus: Option[String],
                   renewalNoticeIssuedDate: Option[String],
                   renewalNoticeFirstSpecifiedDate: Option[String],
                   renewalFormType: Option[String] = None)

object Renewal {
  implicit val formats: Format[Renewal] = Json.format[Renewal]
}
