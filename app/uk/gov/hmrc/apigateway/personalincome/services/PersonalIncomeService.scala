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

package uk.gov.hmrc.apigateway.personalincome.services

import play.api.libs.json.Json
import uk.gov.hmrc.apigateway.personalincome.config.MicroserviceAuditConnector
import uk.gov.hmrc.apigateway.personalincome.connectors._
import uk.gov.hmrc.apigateway.personalincome.domain._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait PersonalIncomeService {
  def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails]

  // Renewal specific - authenticateRenewal, claimantDetails, submitRenewal.
  def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier): Future[Option[TcrAuthenticationToken]]

  def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier): Future[ClaimantDetails]

  def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier): Future[Response]
}

trait LivePersonalIncomeService extends PersonalIncomeService {
  def authConnector: AuthConnector

  def taiConnector: TaiConnector

  def ntcConnector: NtcConnector

  def audit(method:String, details:Map[String, String])(implicit hc: HeaderCarrier) = {
    def auditResponse(): Unit = {
      MicroserviceAuditConnector.sendEvent(
        DataEvent("personal-income", "ServiceResponseSent",
          tags = Map("transactionName" -> method),
          detail = details))
    }
  }

  def withAudit[T](service:String, details:Map[String, String])(func:Future[T])(implicit hc:HeaderCarrier) = {
    audit(service, details) // No need to wait!
    func
  }

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    withAudit("getSummary", Map("nino" -> nino.value, "year" -> year.toString)){taiConnector.taxSummary(nino, year)}
  }

  // Note: The TcrAuthenticationToken must be supplied to claimantDetails and submitRenewal.
  override def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier): Future[Option[TcrAuthenticationToken]] = {
    withAudit("submitRenewal", Map("nino" -> nino.value)) {
      ntcConnector.authenticateRenewal(TaxCreditsNino(nino.value), tcrRenewalReference)
    }
  }

  override def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier): Future[ClaimantDetails] = {
    withAudit("getSummary", Map("nino" -> nino.value)) {
      ntcConnector.claimantDetails(TaxCreditsNino(nino.value))
    }
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier): Future[Response] = {
    withAudit("submitRenewal", Map("nino" -> nino.value)) {
      ntcConnector.submitRenewal(TaxCreditsNino(nino.value), tcrRenewal)
    }
  }

}

object SandboxPersonalIncomeService extends PersonalIncomeService with FileResource {

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    val resource: Option[String] = findResource(s"/resources/getsummary/${nino.value}_$year.json")
    Future.successful(resource.fold(TaxSummaryDetails(nino.value, year)) { found =>
      Json.parse(found).as[TaxSummaryDetails]
    })
  }

  override def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier): Future[Option[TcrAuthenticationToken]] = {
    Future.successful(Some(TcrAuthenticationToken("some-token")))
  }

  override def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier): Future[ClaimantDetails] = {
    Future.successful(ClaimantDetails(false, 1, "renewalForm", nino.value, None, false, "some-app-id"))
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier): Future[Response] = {
    Future.successful(Success(200))
  }

}

object LivePersonalIncomeService extends LivePersonalIncomeService {
  override val authConnector: AuthConnector = AuthConnector

  override val taiConnector = TaiConnector

  override val ntcConnector = NtcConnector
}
