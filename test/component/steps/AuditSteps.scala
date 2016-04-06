/*
 * Copyright 2016 HM Revenue & Customs
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

package component.steps

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import cucumber.api.DataTable
import cucumber.api.scala.{EN, ScalaDsl}
import org.joda.time.DateTime
import org.scalatest.Matchers
import play.api.libs.json._
import uk.gov.hmrc.play.audit.model._

import scala.collection.JavaConversions._

class AuditSteps extends ScalaDsl with EN with Matchers {

  implicit val dateTimeReads = DateTimeReads.dateTimeReads
  implicit val dataCallFormat = Json.format[DataCall]
  implicit val mergedDataEventFormat = Json.format[MergedDataEvent]
  implicit val dataEventFormat = Json.format[DataEvent]

  When( """^The auditing service is up$""") { () =>
    Auditing.create()
  }

  When( """^The auditing service is in error$""") { () =>
    Auditing.error()
  }

  Then( """^a simple event with source '(.+)' and type '(.+)' has been audited with:$""") { (auditSource: String, auditType: String, data: DataTable) =>
     eventHasBeenAudited(auditSource, auditType, data)
  }

  Then( """^No event has been audited$""") { () =>
    withClue(s"There should have been 0 event audited, but found ${allSimpleEvents().size}\n") {
      allSimpleEvents().isEmpty shouldBe true
    }
  }

  private def eventHasBeenAudited(auditSource: String, auditType: String, data: DataTable) = {
    val expectedData = tupple3(data)

    val tags = expectedData collect { case("tags", k, v) => (k,v) }
    val detail = expectedData collect { case("detail", k, v) => (k,v) }

    val eventToFind = DataEvent(auditSource, auditType).withTags(tags: _*).withDetail(detail: _*)

    val unmatchedCriteria = allSimpleEvents().map(event => findSimpleMismatches(eventToFind, event)).sortBy(_.size)

    withClue(
      s"""Could not find this event: $eventToFind
         | - These are the unmatched criteria of the likeliest match: ${unmatchedCriteria.headOption.getOrElse(Iterable.empty)}
         | - These are non-merged events with the same source and type: \n ${allSimpleEventsFor(auditSource, auditType).mkString("\n")}""".stripMargin) {
      unmatchedCriteria.headOption shouldBe Some(Iterable.empty)
    }
  }

  private def findSimpleMismatches(expected: DataEvent, actual: DataEvent): Iterable[(String, String, String)] = {
    findUnmatchedCriteria("tags", expected.tags, actual.tags) ++
    findUnmatchedCriteria("detail", expected.detail, actual.detail)
  }

  private def findUnmatchedCriteria(path: String, expected: Map[String, String], actual: Map[String, String]): Iterable[(String, String, String)] =
    expected.collect {
      case (k,v) if !matchesValue(v, actual.get(k)) => (path + "." + k, v, actual.get(k).toString)
    }

  private def allSimpleEvents(): Seq[DataEvent] = {
    val auditRequests = WireMock.findAll(postRequestedFor(urlMatching("/write/audit")))
    auditRequests map { req => Json.parse(req.getBodyAsString).as[DataEvent]}
  }

  private def allSimpleEventsFor(auditSource: String, auditType: String) = allSimpleEvents() filter (a => a.auditSource == auditSource && a.auditType == auditType)

  val Regex = "regex=(.*)".r
  val EndsWith = "endsWith=(.*)".r
  private def matchesValue(expected: String, actualO: Option[String]): Boolean = {
    (expected, actualO) match {
      case ("None", _) => actualO.isEmpty
      case (Regex(expression), Some(actual)) => actual.matches(s"(?s)$expression")
      case (EndsWith(suffix), Some(actual)) => actual.endsWith(suffix)
      case (plainText, Some(actual)) => actual.contains(plainText)
      case _ => false
    }
  }

  private def tupple3(dataTable: DataTable): Seq[(String,String,String)] = dataTable.raw() map (x => (x(0), x(1), x(2)))
}

object DateTimeReads {
  implicit def dateTimeReads = new Reads[DateTime] {
    def reads(value: JsValue): JsResult[DateTime] = JsSuccess(DateTime.parse(value.as[String]))
  }
}
