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

import com.github.nscala_time.time.Imports._
import com.ning.http.util.Base64
import play.api.Logger
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import uk.gov.hmrc.api.sandbox._
import uk.gov.hmrc.api.service._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.config.AppContext.RenewalStatusTransform
import uk.gov.hmrc.personalincome.config.{AppContext, MicroserviceAuditConnector}
import uk.gov.hmrc.personalincome.connectors._
import uk.gov.hmrc.personalincome.controllers.HeaderKeys
import uk.gov.hmrc.personalincome.domain.userdata._
import uk.gov.hmrc.personalincome.domain.{TcrAuthCheck, _}
import uk.gov.hmrc.personalincome.viewmodelfactories.TaxSummaryContainerFactory
import uk.gov.hmrc.personaltaxsummary.domain.{PersonalTaxSummaryContainer, TaxSummaryContainer}
import uk.gov.hmrc.personaltaxsummary.viewmodels.IncomeTaxViewModel
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait PersonalIncomeService {

  def getTaxSummary(nino: Nino, year:Int, journeyId: Option[String] = None)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer]]

  // Renewal specific - authenticateRenewal must be called first to retrieve the authToken before calling claimantDetails, submitRenewal.
  def authenticateRenewal(nino: Nino, tcrRenewalReference:RenewalReference)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Option[TcrAuthenticationToken]]

  def getTaxCreditExclusion(nino: Nino)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Exclusion]

  def claimantDetails(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimantDetails]

  def claimantClaims(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Claims]

  def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response]

  def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[Either[TaxCreditSummaryOld,TaxCreditSummary]]
}

trait LivePersonalIncomeService extends PersonalIncomeService with Auditor with RenewalStatus {
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

  private val formatA = DateTimeFormat.forPattern("yyyy-MM-dd")
  private val formatB = DateTimeFormat.forPattern("yyyyMMdd")

  override def claimantClaims(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Claims] = {
    withAudit("claimantClaims", Map("nino" -> nino.value)) {

      def claimMatch(claim: Claim) = {
        val match1 = claim.household.applicant1.nino == nino.value
        val match2 = claim.household.applicant2.fold(false) { found => found.nino == nino.value }
        match1 || match2
      }

      def dateConversion(date: Option[String], conversion: String => DateTime): Option[String] = {
        val notFound: Option[String] = None
        date.fold(notFound) { found =>
          Try {
            Some(conversion(found).toString("dd/MM/yyyy"))
          }.getOrElse {
            Logger.error(s"Failed to convert input date $found for NINO ${nino.value}. Removing date from response!")
            notFound
          }
        }
      }

      ntcConnector.claimantClaims(TaxCreditsNino(nino.value)).map { claims =>
        claims.references.fold(Claims(None)) { items => {
          val references = items.filter(a => claimMatch(a))
            .map { claim =>
              Claim(claim.household.copy(householdCeasedDate = dateConversion(claim.household.householdCeasedDate, formatB.parseDateTime)),
                Renewal(
                  dateConversion(claim.renewal.awardStartDate, formatA.parseDateTime),
                  dateConversion(claim.renewal.awardEndDate, formatA.parseDateTime),
                  Some(resolveStatus(claim)),
                  dateConversion(claim.renewal.renewalNoticeIssuedDate, formatB.parseDateTime),
                  dateConversion(claim.renewal.renewalNoticeFirstSpecifiedDate, formatB.parseDateTime)))
            }
            Claims(Some(references))
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

  override def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[Either[TaxCreditSummaryOld, TaxCreditSummary]] = {
    withAudit("getTaxCreditSummary", Map("nino" -> nino.value)) {

      val tcNino = TaxCreditsNino(nino.value)

      def getChildrenAge16AndUnder(): Future[Children] = {
        taxCreditBrokerConnector.getChildren(tcNino).map(children =>
          Children(Child.getEligibleChildren(children)))
      }

      val childrenFuture = getChildrenAge16AndUnder
      val partnerDetailsFuture = taxCreditBrokerConnector.getPartnerDetails(tcNino)
      val paymentSummaryFuture = taxCreditBrokerConnector.getPaymentSummary(tcNino)
      val personalDetailsFuture = taxCreditBrokerConnector.getPersonalDetails(tcNino)

      for {
        children <- childrenFuture
        partnerDetails <- partnerDetailsFuture
        paymentSummary <- paymentSummaryFuture
        personalDetails <- personalDetailsFuture
      } yield {
        paymentSummary match {
          case Left(_) => Left(TaxCreditSummaryOld(paymentSummary.left.get, personalDetails, partnerDetails, children))
          case Right(_) => Right(TaxCreditSummary(paymentSummary.right.get, personalDetails, partnerDetails, children))
        }
      }
    }
  }
}

trait RenewalStatus {
  val defaultRenewalStatus = "NOT_SUBMITTED"
  val      awaitingBarcode = "AWAITING_BARCODE"
  val           no_barcode = "000000000000000"
  lazy val transform = AppContext.renewalStatusTransform
    .map(_.map(transform => transform.copy(statusValues = transform.statusValues.map(_.toUpperCase))))

  def renewalStatusTransform: Option[List[RenewalStatusTransform]]

  def defaultRenewalStatusReturned(returned:String) = {
    Logger.warn(s"Failed to resolve renewalStatus $returned against configuration! Returning default status.")
    defaultRenewalStatus
  }

  def resolveStatus(claim:Claim): String = {
    if (claim.household.barcodeReference.equals(no_barcode)) {
      awaitingBarcode
    } else {
      claim.renewal.renewalStatus.fold(defaultRenewalStatus) { renewalStatus =>
        val config: List[RenewalStatusTransform] = transform.getOrElse(throw new IllegalArgumentException("Failed to resolve renewal status config!"))
        config.flatMap { (item: RenewalStatusTransform) =>
          if (item.statusValues.contains(renewalStatus.toUpperCase.trim)) Some(item.name) else None
        }.headOption.getOrElse(defaultRenewalStatusReturned(renewalStatus))
      }
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
        case ex:Exception => Future.successful(ClaimantDetails(hasPartner = false, 1, "r", "false", None, availableForCOCAutomation = false, "some-app-id"))
      }
    }
  }

  override def claimantClaims(nino: Nino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Claims] = {
    val resource :String = findResource(s"/resources/claimantdetails/claims.json").getOrElse(throw new IllegalArgumentException("Resource not found!"))
    Future.successful(Json.parse(resource).as[Claims])
  }

  override def submitRenewal(nino: Nino, tcrRenewal:TcrRenewal)(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[Response] = {
    Future.successful(uk.gov.hmrc.personalincome.connectors.Success(200))
  }

  override def getTaxCreditSummary(nino:Nino)(implicit hc:HeaderCarrier, ex: ExecutionContext): Future[Either[TaxCreditSummaryOld, TaxCreditSummary]] = {
    val resource :String = findResource(s"/resources/taxcreditsummary/${nino.value}.json").getOrElse(throw new IllegalArgumentException("Resource not found!"))
    val response = Json.parse(resource).validate[TaxCreditSummaryOld] match {
      case success: JsSuccess[TaxCreditSummaryOld] => Left(success.get)
      case _ => Right(Json.parse(resource).as[TaxCreditSummary])
    }
    Future.successful(response)
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

  override def renewalStatusTransform: Option[List[RenewalStatusTransform]] = AppContext.renewalStatusTransform
}
