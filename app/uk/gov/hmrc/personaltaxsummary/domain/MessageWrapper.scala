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

package uk.gov.hmrc.personaltaxsummary.domain

import play.api.libs.json.Json

case class MessageWrapper(a:String, b:String, c:Option[String]=None)

object MessageWrapper {
  implicit val format = Json.format[MessageWrapper]

  def applyForList2(a: List[(String, String)]) : List[MessageWrapper] = {
    a.map(item => MessageWrapper(item._1, item._2, None))
  }

  def applyForList3(a: List[(String, String, String)]) : List[MessageWrapper] = {
    a.map(item => MessageWrapper(item._1, item._2, Some(item._3)))
  }
}

case class BenefitsDataWrapper(a:String, b:String, c:String, d:String, e:Option[Int], f:Option[Int])

object BenefitsDataWrapper {
  implicit val format = Json.format[BenefitsDataWrapper]

  def applyBenefit(a:List[(String, String, String, String, Option[Int], Option[Int])]) : List[BenefitsDataWrapper] = {
    a.map(item => BenefitsDataWrapper(item._1, item._2, item._3, item._4, item._5, item._6))
  }
}
