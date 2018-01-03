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

import org.joda.time.{LocalDate, Years, DateTime}
import play.api.libs.json.Json

case class Child(firstNames:String,
                 surname:String,
                 dateOfBirth:DateTime,
                 hasFTNAE:Boolean,
                 hasConnexions: Boolean,
                 isActive:Boolean,
                 dateOfDeath:Option[DateTime])


object Child {
  implicit val formats = Json.format[Child]

  def getAge(child:Child) = {
    val years = Years.yearsBetween(new LocalDate(child.dateOfBirth), new LocalDate())
    years.getYears();
  }

  def getEligibleChildren(children: Children): Seq[Child] = {
    children.child.filter { child =>
      getAge(child) < 20 &&
        child.isActive &&
        !child.dateOfDeath.isDefined
    }
  }
}
