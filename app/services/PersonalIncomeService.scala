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

package services

import config.MicroserviceAuditConnector
import connectors.TaiConnector
import domain._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait PersonalIncomeService {
  def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails]
}

trait LivePersonalIncomeService extends PersonalIncomeService {
  val connector: TaiConnector

  def audit(method:String, nino:Nino, year:Int, details:Map[String, String]) = {
    def auditResponse(): Unit = {
      MicroserviceAuditConnector.sendEvent(
        DataEvent("personal-income", "ServiceResponseSent",
          tags = Map("transactionName" -> method),
          detail = details))
    }
  }

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    connector.taxSummary(nino, year).map {
      resp => audit("getSummary", nino, year, Map("nino" -> nino.value, "year" -> year.toString))
        resp
    }
  }
}

object SandboxExampleService extends PersonalIncomeService {
  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    Future.successful(TaxSummaryDetails("somevalue", 1))
  }
}

object LivePersonalIncomeService extends LivePersonalIncomeService {
  override val connector = TaiConnector
}
