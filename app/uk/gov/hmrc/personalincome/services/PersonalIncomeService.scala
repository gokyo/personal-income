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

package uk.gov.hmrc.personalincome.services

import com.ning.http.util.Base64
import play.api.Logger
import play.api.libs.json.{JsError, Json}
import uk.gov.hmrc.api.sandbox._
import uk.gov.hmrc.api.service._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.config.MicroserviceAuditConnector
import uk.gov.hmrc.personalincome.connectors._
import uk.gov.hmrc.personalincome.controllers.HeaderKeys
import uk.gov.hmrc.personalincome.domain.userdata.{Child, Children, Exclusion, TaxCreditSummary}
import uk.gov.hmrc.personalincome.domain.{TcrAuthCheck, _}
import uk.gov.hmrc.personalincome.viewmodelfactories.TaxSummaryContainerFactory
import uk.gov.hmrc.personaltaxsummary.domain.{PersonalTaxSummaryContainer, TaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.viewmodels.IncomeTaxViewModel
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait PersonalIncomeService {

  def getTaxSummary(nino: Nino, year:Int, journeyId: Option[String] = None)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer]]

  // Renewal specific - authenticateRenewal must be called first to retrieve the authToken before calling claimantDetails, submitRenewal.
  def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TcrAuthenticationToken]]

  def getTaxCreditExclusion(nino: Nino)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Exclusion]

  def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimantDetails]

  def claimantClaims(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimsWithRef]

  def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response]

  def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[TaxCreditSummary]
}

trait LivePersonalIncomeService extends PersonalIncomeService with Auditor {
  def authConnector: AuthConnector

  def personalTaxSummaryConnector: PersonalTaxSummaryConnector

  def taiConnector: TaiConnector

  def ntcConnector: NtcConnector

  def taxCreditBrokerConnector: TaxCreditsBrokerConnector

  def gateKeepered(taxSummary: TaxSummaryDetails): Boolean = {
    taxSummary.gateKeeper.exists(_.gateKeepered)
  }


  override def getTaxSummary(nino: Nino, year: Int, journeyId: Option[String] = None)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TaxSummaryContainer]] = {
    withAudit("getTaxSummary", Map("nino" -> nino.value, "year" -> year.toString)) {
      taiConnector.taxSummary(nino, year).flatMap {
        case Some(taiTaxSummary) => buildTaxSummary(nino, journeyId, taiTaxSummary).map(Option(_))
        case None => Future successful None
      }
    }
  }

  private def buildTaxSummary(nino: Nino, journeyId: Option[String], taxSummary: TaxSummaryDetails)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[TaxSummaryContainer] = {
    val personalTaxSummaryContainer = PersonalTaxSummaryContainer(taxSummary, Map.empty)
    for {
      estimatedIncome <- personalTaxSummaryConnector.buildEstimatedIncome(nino, personalTaxSummaryContainer, journeyId)
      yourTaxableIncome <- personalTaxSummaryConnector.buildYourTaxableIncome(nino, personalTaxSummaryContainer, journeyId)
    } yield {
      TaxSummaryContainerFactory.buildTaxSummaryContainer(nino, taxSummary,estimatedIncome, yourTaxableIncome)
    }
  }

  // Note: The TcrAuthenticationToken must be supplied to claimantDetails and submitRenewal.
  override def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TcrAuthenticationToken]] = {
    withAudit("authenticateRenewal", Map("nino" -> nino.value)) {
      ntcConnector.authenticateRenewal(TaxCreditsNino(nino.value), tcrRenewalReference)
    }
  }

  override def getTaxCreditExclusion(nino: Nino)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Exclusion] = {
    withAudit("getTaxCreditExclusion", Map("nino" -> nino.value)) {
      taxCreditBrokerConnector.getExclusion(TaxCreditsNino(nino.value))
    }
  }

  override def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimantDetails] = {
    withAudit("claimantDetails", Map("nino" -> nino.value)) {
      ntcConnector.claimantDetails(TaxCreditsNino(nino.value))
    }
  }

  override def claimantClaims(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimsWithRef] = {
    withAudit("claimantClaims", Map("nino" -> nino.value)) {

      def claimMatch(claim:Claim) = {
        val match1 = claim.household.applicant1.nino == nino.value
        val match2 = claim.household.applicant2.fold(false){found => found.nino == nino.value}
        match1 | match2
      }

      ntcConnector.claimantClaims(TaxCreditsNino(nino.value)).map { claims =>
        claims.references.fold(ClaimsWithRef(None)){ items => {
            val assoicatedReferences = items.filter(a => claimMatch(a))
              .map(b =>  ClaimWithReference(b.household, b.renewal, TcrAuthenticationToken.basicAuthString(nino.value, b.household.barcodeReference)))
            ClaimsWithRef(Some(assoicatedReferences))
          }
        }
      }
    }
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response] = {
    withAudit("submitRenewal", Map("nino" -> nino.value)) {
      ntcConnector.submitRenewal(TaxCreditsNino(nino.value), tcrRenewal)
    }
  }

  override def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[TaxCreditSummary] = {
    withAudit("getTaxCreditSummary", Map("nino" -> nino.value)) {

      val tcNino = TaxCreditsNino(nino.value)

      def getChildrenAge16AndUnder(): Future[Children] = {
        taxCreditBrokerConnector.getChildren(tcNino).map(children =>
          Children(children.child.filter(child => Child.getAge(child) <= 16)))
      }

      val childrenFuture = getChildrenAge16AndUnder
      val parterDetailsFuture = taxCreditBrokerConnector.getPartnerDetails(tcNino)
      val paymentSummaryFuture = taxCreditBrokerConnector.getPaymentSummary(tcNino)
      val personalDetailsFuture = taxCreditBrokerConnector.getPersonalDetails(tcNino)

      for {
        children <- childrenFuture
        parterDetails <- parterDetailsFuture
        paymentSummary <- paymentSummaryFuture
        personalDetails <- personalDetailsFuture
      } yield(TaxCreditSummary(paymentSummary, personalDetails, parterDetails, children))
    }
  }
}

object SandboxPersonalIncomeService extends PersonalIncomeService with FileResource {
  override def getTaxSummary(nino: Nino, year: Int, journeyId: Option[String] = None)(implicit hc: HeaderCarrier, ex: ExecutionContext) = {
    val resource: Option[String] = findResource(s"/resources/getsummary/${nino.value}_${year}_refresh.json")

    val details: TaxSummaryDetailsResponse = TaxSummaryDetailsResponse(nino.value, 1)
    val baseViewModel: IncomeTaxViewModel = IncomeTaxViewModel(simpleTaxUser = true)
    val taxSummaryContainerNew = uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer(details, baseViewModel, None, None, None)

    val summary = resource.fold(taxSummaryContainerNew) { found =>
      Json.parse(found).validate[uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer].fold(
        error => {
          Logger.error("Failed to parse summary " + JsError.toJson(error))
          throw new Exception("Failed to validate JSON data for summary!")
        },
        result => {
          result
        }
      )
    }
    Future.successful(Some(summary))
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
        Future.successful(Json.parse(resource).as[ClaimantDetails])
      } catch {
        case ex:Exception => Future.successful(ClaimantDetails(hasPartner = false, 1, "r", nino.value, None, availableForCOCAutomation = false, "some-app-id"))
      }
    }
  }

  override def claimantClaims(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimsWithRef] = {
    val resource :String = findResource(s"/resources/claimantdetails/claims.json").getOrElse(throw new IllegalArgumentException("Resource not found!"))
    Future.successful(Json.parse(resource).as[ClaimsWithRef])
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response] = {
    Future.successful(uk.gov.hmrc.personalincome.connectors.Success(200))
  }

  override def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[TaxCreditSummary] = {
    val resource :String = findResource(s"/resources/taxcreditsummary/${nino.value}.json").getOrElse(throw new IllegalArgumentException("Resource not found!"))
    Future.successful(Json.parse(resource).as[TaxCreditSummary])
  }

  override def getTaxCreditExclusion(nino: Nino)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Exclusion] = Future.successful(Exclusion(false))

}

object LivePersonalIncomeService extends LivePersonalIncomeService {
  override val authConnector: AuthConnector = AuthConnector

  override val personalTaxSummaryConnector = PersonalTaxSummaryConnector

  override val taiConnector = TaiConnector

  override val ntcConnector = NtcConnector

  override val taxCreditBrokerConnector = TaxCreditsBrokerConnector

  override val auditConnector: AuditConnector = MicroserviceAuditConnector
}
