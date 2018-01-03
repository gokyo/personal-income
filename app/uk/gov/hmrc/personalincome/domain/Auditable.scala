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

trait Auditable {
    def txmAuditType: String
    def readableFormat : String


  def dataForAudit(tagOverrides : Map[String,String] = auditableTagNameOverrides): Map[String, String] =  {

    this.getClass.getDeclaredFields.filterNot(f => excludedAuditFields.exists(exclude =>  exclude == f.getName)).map {
      field =>
        field.setAccessible(true)
        (getAuditableFieldName(field.getName, tagOverrides) ->  fieldValueMapper(field.get(this)))
    }.toMap
  }

  private def getAuditableFieldName(fieldName : String, tagOverrides : Map[String,String]) : String = {
    tagOverrides.getOrElse(fieldName, fieldName)
  }

  def fieldValueMapper(fieldValue : Object) : String = {
    fieldValue match {
      case Some(a) => a.toString
      case None => ""
      case a => a.toString
    }
  }

  def auditableTagNameOverrides : Map[String,String]  = Map()

  val excludedAuditFields = List("auditableTagNameOverrides", "excludedAuditFields","txmAuditType")
}

case class ExplicitTypeOfChange(txmAuditType: String) extends Auditable {
  override def readableFormat: String = {
    s"Type of Change=${txmAuditType}"
  }
}
