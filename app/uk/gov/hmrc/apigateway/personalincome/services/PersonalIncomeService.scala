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

import play.api.Logger
import play.api.libs.json.Json
import uk.gov.hmrc.apigateway.personalincome.config.MicroserviceAuditConnector
import uk.gov.hmrc.apigateway.personalincome.connectors._
import uk.gov.hmrc.apigateway.personalincome.domain._
import uk.gov.hmrc.apigateway.personalincome.domain.userdata.RenewalSummary
import uk.gov.hmrc.apigateway.personalincome.utils.TaxSummaryHelper
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.api.service._
import uk.gov.hmrc.api.sandbox._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure, Try}

trait PersonalIncomeService {
  def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryContainer]

  // Renewal specific - authenticateRenewal must be called first to retrieve the authToken before calling claimantDetails, submitRenewal.
  def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier): Future[Option[TcrAuthenticationToken]]

  def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier): Future[ClaimantDetails]

  def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier): Future[Response]

  def getRenewalSummary(nino:Nino)(implicit hc:HeaderCarrier): Future[RenewalSummary]
}

trait LivePersonalIncomeService extends PersonalIncomeService with Auditor {
  def authConnector: AuthConnector

  def taiConnector: TaiConnector

  def ntcConnector: NtcConnector

  def taxCreditBrokerConnector: TaxCreditsBrokerConnector

  def gateKeepered(taxSummary: TaxSummaryDetails): Boolean = {
    taxSummary.gateKeeper.exists(_.gateKeepered)
  }

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryContainer] = {
    withAudit("getSummary", Map("nino" -> nino.value, "year" -> year.toString)) {

      taiConnector.taxSummary(nino, year).map(summary =>
        gateKeepered(summary) match {
          case false =>
            val estimatedIncomeWrapper = Try(EstimatedIncomePageVM.createObject(nino, summary)) match {
              case Success(value) => Some(EstimatedIncomeWrapper(value, TaxSummaryHelper.getPotentialUnderpayment(summary)))
              case Failure(failure) =>
                Logger.error(s"Failed to create EstimatedIncome! Failure is $failure")
                None
            }

            TaxSummaryContainer(summary,
              BaseViewModelVM.createObject(nino, summary),
              estimatedIncomeWrapper,
              Some(YourTaxableIncomePageVM.createObject(nino, summary)),
              None)

          case true =>
            TaxSummaryContainer(summary,
              BaseViewModelVM.createObject(nino, summary),
              None,
              None,
              Some(GateKeeperPageVM.createObject(nino, summary)))
        })
    }
  }

  // Note: The TcrAuthenticationToken must be supplied to claimantDetails and submitRenewal.
  override def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier): Future[Option[TcrAuthenticationToken]] = {
    withAudit("authenticateRenewal", Map("nino" -> nino.value)) {
      ntcConnector.authenticateRenewal(TaxCreditsNino(nino.value), tcrRenewalReference)
    }
  }

  override def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier): Future[ClaimantDetails] = {
    withAudit("claimantDetails", Map("nino" -> nino.value)) {
      ntcConnector.claimantDetails(TaxCreditsNino(nino.value))
    }
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier): Future[Response] = {
    withAudit("submitRenewal", Map("nino" -> nino.value)) {
      ntcConnector.submitRenewal(TaxCreditsNino(nino.value), tcrRenewal)
    }
  }

  override def getRenewalSummary(nino:Nino)(implicit hc:HeaderCarrier): Future[RenewalSummary] = {
    withAudit("submitRenewal", Map("nino" -> nino.value)) {
      for {
        children <- taxCreditBrokerConnector.getChildren(TaxCreditsNino(nino.value))
        parterDetails <- taxCreditBrokerConnector.getPartnerDetails(TaxCreditsNino(nino.value))
        paymentSummary <- taxCreditBrokerConnector.getPaymentSummary(TaxCreditsNino(nino.value))
        personalDetails <- taxCreditBrokerConnector.getPersonalDetails(TaxCreditsNino(nino.value))
      } yield(RenewalSummary(paymentSummary, personalDetails, parterDetails, children))
    }
  }

}

object SandboxPersonalIncomeService extends PersonalIncomeService with FileResource {

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryContainer] = {
    val resource: Option[String] = findResource(s"/resources/getsummary/${nino.value}_$year.json")

    Future.successful(resource.fold(TaxSummaryContainer(TaxSummaryDetails(nino.value, year), BaseViewModel(estimatedIncomeTax=0), None, None, None)) { found =>
      Json.parse(found).as[TaxSummaryContainer]
    })
  }

  override def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier): Future[Option[TcrAuthenticationToken]] = {
    Future.successful(Some(TcrAuthenticationToken("some-token")))
  }

  override def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier): Future[ClaimantDetails] = {
    Future.successful(ClaimantDetails(hasPartner=false, 1, "renewalForm", nino.value, None, availableForCOCAutomation=false, "some-app-id"))
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier): Future[Response] = {
    Future.successful(uk.gov.hmrc.apigateway.personalincome.connectors.Success(200))
  }

  override def getRenewalSummary(nino:Nino)(implicit hc:HeaderCarrier): Future[RenewalSummary] = {
    val resource :String = findResource(s"/resources/renewalsummary/${nino.value}.json").getOrElse(throw new IllegalArgumentException("Resource not found!"))
    Future.successful(Json.parse(resource).as[RenewalSummary])
  }

}

object LivePersonalIncomeService extends LivePersonalIncomeService {
  override val authConnector: AuthConnector = AuthConnector

  override val taiConnector = TaiConnector

  override val ntcConnector = NtcConnector

  override val taxCreditBrokerConnector = TaxCreditsBrokerConnector

  override val auditConnector: AuditConnector = MicroserviceAuditConnector
}
