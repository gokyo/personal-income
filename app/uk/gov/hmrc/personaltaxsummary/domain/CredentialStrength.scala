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

package uk.gov.hmrc.personaltaxsummary.domain

import play.api.libs.json._

sealed abstract class CredentialStrength(val name: String)

object CredentialStrength {

  case object Strong extends CredentialStrength("strong")
  case object Weak extends CredentialStrength("weak")
  case object None extends CredentialStrength("none")

  val fromName: String => CredentialStrength = Seq(Strong, Weak, None).map(c => c.name -> c).toMap

  implicit val format = {
    val reads = new Reads[CredentialStrength] {
      override def reads(json: JsValue) =
        try {
          JsSuccess(fromName(json.as[String]))
        } catch {
          case _ : Throwable => JsError()
        }
    }
    val writes = new Writes[CredentialStrength] {
      override def writes(o: CredentialStrength) = new JsString(o.name)
    }
    Format(reads, writes)
  }

}
