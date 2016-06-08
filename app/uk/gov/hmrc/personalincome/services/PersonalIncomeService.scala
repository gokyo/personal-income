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

package uk.gov.hmrc.personalincome.services

import com.ning.http.util.Base64
import play.api.Logger
import play.api.libs.json.{JsResult, JsError, Json}
import uk.gov.hmrc.personalincome.config.MicroserviceAuditConnector
import uk.gov.hmrc.personalincome.connectors._
import uk.gov.hmrc.personalincome.controllers.HeaderKeys
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.personalincome.domain.userdata.TaxCreditSummary
import uk.gov.hmrc.personalincome.utils.TaxSummaryHelper
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.api.service._
import uk.gov.hmrc.api.sandbox._
import uk.gov.hmrc.personalincome.domain.TcrAuthCheck

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

trait PersonalIncomeService {
  def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[TaxSummaryContainer]

  // Renewal specific - authenticateRenewal must be called first to retrieve the authToken before calling claimantDetails, submitRenewal.
  def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TcrAuthenticationToken]]

  def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimantDetails]

  def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response]

  def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[TaxCreditSummary]
}

trait LivePersonalIncomeService extends PersonalIncomeService with Auditor {
  def authConnector: AuthConnector

  def taiConnector: TaiConnector

  def ntcConnector: NtcConnector

  def taxCreditBrokerConnector: TaxCreditsBrokerConnector

  def gateKeepered(taxSummary: TaxSummaryDetails): Boolean = {
    taxSummary.gateKeeper.exists(_.gateKeepered)
  }

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[TaxSummaryContainer] = {
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
  override def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TcrAuthenticationToken]] = {
    withAudit("authenticateRenewal", Map("nino" -> nino.value)) {
      ntcConnector.authenticateRenewal(TaxCreditsNino(nino.value), tcrRenewalReference)
    }
  }

  override def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimantDetails] = {
    withAudit("claimantDetails", Map("nino" -> nino.value)) {
      ntcConnector.claimantDetails(TaxCreditsNino(nino.value))
    }
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response] = {
    withAudit("submitRenewal", Map("nino" -> nino.value)) {
      ntcConnector.submitRenewal(TaxCreditsNino(nino.value), tcrRenewal)
    }
  }

  override def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[TaxCreditSummary] = {
    withAudit("getTaxCreditSummary", Map("nino" -> nino.value)) {
      for {
        children <- taxCreditBrokerConnector.getChildren(TaxCreditsNino(nino.value))
        parterDetails <- taxCreditBrokerConnector.getPartnerDetails(TaxCreditsNino(nino.value))
        paymentSummary <- taxCreditBrokerConnector.getPaymentSummary(TaxCreditsNino(nino.value))
        personalDetails <- taxCreditBrokerConnector.getPersonalDetails(TaxCreditsNino(nino.value))
      } yield(TaxCreditSummary(paymentSummary, personalDetails, parterDetails, children))
    }
  }
}

object SandboxPersonalIncomeService extends PersonalIncomeService with FileResource {

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[TaxSummaryContainer] = {
    val resource: Option[String] = findResource(s"/resources/getsummary/${nino.value}_$year.json")

    Future.successful(resource.fold(TaxSummaryContainer(TaxSummaryDetails(nino.value, year), BaseViewModel(estimatedIncomeTax=0), None, None, None)) { found =>
      Json.parse(found).validate[TaxSummaryContainer].fold(
        error => {
          Logger.error("Failed to parse summary " + JsError.toFlatJson(error))
          throw new Exception("Failed to validate JSON data for summary!")
        },
        result => {
          result
        }
      )
    })
  }

  private def basicAuthString(encodedAuth:String): String = "Basic " + encodedAuth
  private def encodedAuth(nino: Nino, tcrRenewalReference:RenewalReference): String = new String(Base64.encode(s"${nino.value}:${tcrRenewalReference.value}".getBytes))
  private def getTcrAuthHeader[T](func: TcrAuthenticationToken => T)(implicit headerCarrier: HeaderCarrier): T = {
    headerCarrier.extraHeaders.headOption match {
      case Some((HeaderKeys.tcrAuthToken , t@TcrAuthCheck(_))) => func(TcrAuthenticationToken(t))
      case _ => throw new IllegalArgumentException("Failed to locate tcrAuthToken")
    }
  }

  override def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TcrAuthenticationToken]] = {
    Future.successful(Some(TcrAuthenticationToken(basicAuthString(encodedAuth(nino, tcrRenewalReference)))))
  }

  override def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimantDetails] = {
    getTcrAuthHeader { header =>
      try {
        val resource: String = findResource(s"/resources/claimantdetails/${nino.value}-${header.extractRenewalReference.get}.json").getOrElse(throw new IllegalArgumentException("Resource not found!"))
        val resp = Json.parse(resource).as[ClaimantDetails]
        Future.successful(resp)
      } catch {
        case ex:Exception => Future.successful(ClaimantDetails(hasPartner = false, 1, "r", nino.value, None, availableForCOCAutomation = false, "some-app-id"))
      }
    }
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response] = {
    Future.successful(uk.gov.hmrc.personalincome.connectors.Success(200))
  }

  override def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[TaxCreditSummary] = {
    val resource :String = findResource(s"/resources/taxcreditsummary/${nino.value}.json").getOrElse(throw new IllegalArgumentException("Resource not found!"))
    Future.successful(Json.parse(resource).as[TaxCreditSummary])
  }

}

object LivePersonalIncomeService extends LivePersonalIncomeService {
  override val authConnector: AuthConnector = AuthConnector

  override val taiConnector = TaiConnector

  override val ntcConnector = NtcConnector

  override val taxCreditBrokerConnector = TaxCreditsBrokerConnector

  override val auditConnector: AuditConnector = MicroserviceAuditConnector
}
