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

package uk.gov.hmrc.personalincome.controllers

import java.time.LocalDate._

import com.ning.http.util.Base64
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito
import org.mockito.Mockito.{times, verify, when}
import org.mockito.stubbing.OngoingStubbing
import org.scalatest.mockito.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.ConfidenceLevel._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.{Retrieval, ~}
import uk.gov.hmrc.auth.core.syntax.retrieved._
import uk.gov.hmrc.auth.core.{AuthConnector, ConfidenceLevel}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.personalincome.config.AppContext
import uk.gov.hmrc.personalincome.connectors.{NtcConnector, PersonalTaxSummaryConnector, TaiConnector, TaxCreditsBrokerConnector}
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.personalincome.services.{LivePersonalIncomeService, PersonalIncomeService}
import uk.gov.hmrc.personaltaxsummary.domain.TaxSummaryContainer
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

trait TestSetup extends MockitoSugar with UnitSpec {

  val nino = "CS700100A"
  val incorrectNino = Nino("SC100700A")
  val renewalReference = RenewalReference("111111111111111")
  val tcrAuthToken = TcrAuthenticationToken("some-auth-token")
  val acceptHeader = "Accept" -> "application/vnd.hmrc.1.0+json"
  lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest().withSession(
    "AuthToken" -> "Some Header"
  ).withHeaders(
    acceptHeader,
    "Authorization" -> "Some Header"
  )

  trait mocks {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    implicit val mockAuthConnector: AuthConnector = mock[AuthConnector]
    implicit val mockNtcConnector: NtcConnector = mock[NtcConnector]
    implicit val mockTaiConnector: TaiConnector = mock[TaiConnector]
    implicit val mockPersonalTaxSummaryConnector: PersonalTaxSummaryConnector = mock[PersonalTaxSummaryConnector]
    implicit val mockTaxCreditsBrokerConnector: TaxCreditsBrokerConnector =  mock[TaxCreditsBrokerConnector]
    implicit val mockAuditConnector: AuditConnector = mock[AuditConnector]
    implicit val mockLivePersonalIncomeService: PersonalIncomeService = mock[LivePersonalIncomeService]

  }

  type GrantAccess = Option[String] ~ ConfidenceLevel
  def stubAuthorisationGrantAccess(response: GrantAccess)(implicit authConnector: AuthConnector): OngoingStubbing[Future[GrantAccess]] = {
    when(authConnector.authorise(any[Predicate](), any[Retrieval[GrantAccess]]())(any[HeaderCarrier](), any[ExecutionContext]()))
      .thenReturn(Future.successful(response))
  }

  def stubGetSummaryResponse(response: Option[TaxSummaryContainer])(implicit personalIncomeService: PersonalIncomeService): OngoingStubbing[Future[Option[TaxSummaryContainer]]] = {
    when(personalIncomeService.getTaxSummary(any[Nino](), any[Int](), any[Option[String]]())(any[HeaderCarrier](), any[ExecutionContext]()))
      .thenReturn(response)
  }

  def stubAuthRenewalResponse(response: Option[TcrAuthenticationToken])(implicit personalIncomeService: PersonalIncomeService): OngoingStubbing[Future[Option[TcrAuthenticationToken]]] = {
    when(personalIncomeService.authenticateRenewal(any[Nino](),any[RenewalReference]())(any[HeaderCarrier](), any[ExecutionContext]()))
      .thenReturn(response)
  }

  def stubClaimantDetailsResponse(response: ClaimantDetails)(implicit personalIncomeService: PersonalIncomeService) : OngoingStubbing[Future[ClaimantDetails]] = {
    when(personalIncomeService.claimantDetails(any[Nino]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(response)
  }

  def stubClaimantClaimsResponse(response: Claims)(implicit personalIncomeService: PersonalIncomeService) : OngoingStubbing[Future[Claims]] = {
    when(personalIncomeService.claimantClaims(any[Nino]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(response)
  }

  def basicAuthString(encodedAuth:String): String = "Basic " + encodedAuth

  def encodedAuth(nino: Nino, tcrRenewalReference:RenewalReference): String = new String(Base64.encode(s"${nino.value}:${tcrRenewalReference.value}".getBytes))

  def emptyRequestWithAcceptHeaderAndAuthHeader(renewalsRef: RenewalReference, nino: Nino) = FakeRequest().withHeaders(
    acceptHeader, HeaderKeys.tcrAuthToken -> basicAuthString(encodedAuth(nino, renewalsRef)))

  def buildClaims(claims: Claims) = {
    val applicantNotFound: Option[Applicant] = None
    val updated = claims.references.get.map { item =>
      val applicant1 = item.household.applicant1
      val newApp = applicant1.copy(nino = "AM242413B")
      val secondApp: Option[Applicant] = item.household.applicant2.fold(applicantNotFound) { found => Some(found.copy(nino = "AM242413B")) }
      val newHousehold = item.household.copy(applicant1 = newApp, applicant2 = secondApp)
      item.copy(household = newHousehold, renewal = item.renewal)
    }
    Claims(Some(updated))
  }

  class TestPersonalIncomeController(val authConnector: AuthConnector, val service: PersonalIncomeService, val confLevel: Int) extends PersonalIncomeController {
    override val taxCreditsSubmissionControlConfig: TaxCreditsControl = new TaxCreditsControl {
      override def toTaxCreditsSubmissions: TaxCreditsSubmissions = new TaxCreditsSubmissions(false, true, true)
      override def toTaxCreditsRenewalsState: TaxCreditsRenewalsState =
        TaxCreditsRenewalsState(submissionState = true, submissionsState = "open")
    }
    override def getConfigForClaimsMaxAge: Option[Long] = Some(1800)
  }

  class TestPersonalIncomeService(val ntcConnector: NtcConnector,
                                  val taiConnector: TaiConnector,
                                  val personalTaxSummaryConnector: PersonalTaxSummaryConnector,
                                  val taxCreditBrokerConnector: TaxCreditsBrokerConnector,
                                  val auditConnector: AuditConnector) extends LivePersonalIncomeService {
    override val renewalStatusTransform: Option[List[AppContext.RenewalStatusTransform]] = None
  }
}

class TestPersonalIncomeSummarySpec extends TestSetup with WithFakeApplication {

  "getSummary Live" should {

    "return 401 when the nino in the request does not match the authority nino" in new mocks {
      val incorrectNino = Nino("SC100700A")
      stubAuthorisationGrantAccess(Some(nino) and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      status(await(controller.getSummary(incorrectNino, now().getYear, Option("unique-journey-id")).apply(fakeRequest))) shouldBe 401
    }

    "return 404 when summary returned is None" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      stubGetSummaryResponse(None)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      status(await(controller.getSummary(Nino(nino), now().getYear, Option("unique-journey-id")).apply(fakeRequest))) shouldBe 404
    }

    "return unauthorized when authority record does not contain a NINO" in new mocks {
      stubAuthorisationGrantAccess(None and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result: Result = await(controller.getSummary(Nino(nino), now().getYear, Option("unique-journey-id")).apply(fakeRequest))
      status(result) shouldBe 401
      contentAsJson(result) shouldBe Json.parse("""{"code":"UNAUTHORIZED","message":"NINO does not exist on account"}""")
    }

    "return unauthorized when authority record has a low CL" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L100)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result: Result = await(controller.getSummary(Nino(nino), now().getYear, Option("unique-journey-id")).apply(fakeRequest))
      status(result) shouldBe 401
      contentAsJson(result) shouldBe Json.parse("""{"code":"LOW_CONFIDENCE_LEVEL","message":"Confidence Level on account does not allow access"}""")
    }

    "return status code 406 when the headers are invalid" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L100)
      val requestInvalidHeaders: FakeRequest[AnyContentAsEmpty.type] = FakeRequest().withSession(
        "AuthToken" -> "Some Header"
      ).withHeaders(
        "Authorization" -> "Some Header"
      )
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      status(await(controller.getSummary(Nino(nino), now().getYear, Option("unique-journey-id")).apply(requestInvalidHeaders))) shouldBe 406
    }
  }
}

class TestPersonalIncomeRenewalAuthenticateSpec extends TestSetup with WithFakeApplication {


  "authenticate Live" should {

    "process the authentication successfully" in new mocks {
      val tcrAuthToken = TcrAuthenticationToken("some-auth-token")
      stubAuthorisationGrantAccess(Some(nino) and L200)
      stubAuthRenewalResponse(Some(tcrAuthToken))
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result: Result = await(controller.getRenewalAuthentication(Nino(nino), renewalReference).apply(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(tcrAuthToken)
    }

    "return 401 when the nino in the request does not match the authority nino" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      status(await(controller.getRenewalAuthentication(incorrectNino, renewalReference).apply(fakeRequest))) shouldBe 401
    }

    "process the authentication successfully when journeyId is supplied" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      stubAuthRenewalResponse(Some(tcrAuthToken))
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result: Result = await(controller.getRenewalAuthentication(Nino(nino), renewalReference, Some("some-unique-journey-id")).apply(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(tcrAuthToken)
    }

    "return Http 404 (NotFound) response when hod returns either a (BadRequest) 400 or (NotFound) 404 status" in new mocks {
      val renewalReferenceNines = RenewalReference("999999999999999")
      when(mockNtcConnector.authenticateRenewal(any[TaxCreditsNino](), any[RenewalReference]())(any[HeaderCarrier](), any[ExecutionContext]()))
        .thenReturn(Future(None))
      stubAuthorisationGrantAccess(Some(nino) and L200)
      override val mockLivePersonalIncomeService: TestPersonalIncomeService = new TestPersonalIncomeService(mockNtcConnector, mockTaiConnector, mockPersonalTaxSummaryConnector, mockTaxCreditsBrokerConnector, mockAuditConnector)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      status(await(controller.getRenewalAuthentication(Nino(nino), renewalReferenceNines)(fakeRequest))) shouldBe 404
    }

    "return unauthorized when authority record does not contain a NINO" in new mocks {
      stubAuthorisationGrantAccess(None and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result: Result = await(controller.getRenewalAuthentication(Nino(nino), renewalReference, Option("unique-journey-id")).apply(fakeRequest))
      status(result) shouldBe 401
      contentAsJson(result) shouldBe Json.parse("""{"code":"UNAUTHORIZED","message":"NINO does not exist on account"}""")
    }

    "return unauthorized when authority record has a low CL" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L100)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result: Result = await(controller.getRenewalAuthentication(Nino(nino), renewalReference, Option("unique-journey-id")).apply(fakeRequest))
      status(result) shouldBe 401
      contentAsJson(result) shouldBe Json.parse("""{"code":"LOW_CONFIDENCE_LEVEL","message":"Confidence Level on account does not allow access"}""")
    }

    "return status code 406 when the headers are invalid" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L100)
      val requestInvalidHeaders: FakeRequest[AnyContentAsEmpty.type] = FakeRequest().withSession(
        "AuthToken" -> "Some Header"
      ).withHeaders(
        "Authorization" -> "Some Header"
      )
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      status(await(controller.getRenewalAuthentication(Nino(nino), renewalReference, Option("unique-journey-id")).apply(requestInvalidHeaders))) shouldBe 406
    }
  }
}

class TestPersonalIncomeRenewalClaimantDetailsSpec extends TestSetup with WithFakeApplication with ClaimsJson {

  "requesting claimant details Live" should {

    "return claimant details successfully" in new mocks {
      val claimantDetails = ClaimantDetails(false, 1, "r", nino, None, false, "some-app-id")
      stubAuthorisationGrantAccess(Some(nino) and L200)
      stubClaimantDetailsResponse(claimantDetails)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result = await(controller.claimantDetails(Nino(nino)).apply(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference, Nino(nino))))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimantDetails.copy(mainApplicantNino = "true"))
      result.header.headers.get("Cache-Control") shouldBe None
    }

    "return claimant details successfully when NINO does not match mainApplicantNino" in new mocks {
      val claimantDetails = ClaimantDetails(false, 1, "r", nino, None, false, "some-app-id")
      stubAuthorisationGrantAccess(Some(nino) and L200)
      stubClaimantDetailsResponse(claimantDetails)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result = await(controller.claimantDetails(Nino(nino))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference, Nino(nino))))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimantDetails.copy(mainApplicantNino = "false"))
      result.header.headers.get("Cache-Control") shouldBe None
    }

    "return claimant claims successfully" in new mocks {
      val matchedClaims = Json.parse(matchedClaimsJson).as[Claims]
      stubAuthorisationGrantAccess(Some(nino) and L200)
      stubClaimantClaimsResponse(matchedClaims)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result = await(controller.claimantDetails(Nino(nino), None, Some("claims"))(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJson)
      result.header.headers.get("Cache-Control") shouldBe Some("max-age=1800")
    }

    "return claimant claims successfully and drop invalid dates from the response" in new mocks {
      val matchedClaims = Json.parse(claimsJsonWithInvalidDates).as[Claims]
      when(mockNtcConnector.claimantClaims(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]()))
        .thenReturn(Future(matchedClaims))
      stubAuthorisationGrantAccess(Some(nino) and L200)
      override val mockLivePersonalIncomeService: TestPersonalIncomeService = new TestPersonalIncomeService(mockNtcConnector, mockTaiConnector, mockPersonalTaxSummaryConnector, mockTaxCreditsBrokerConnector, mockAuditConnector)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result = await(controller.claimantDetails(Nino(nino), None, Some("claims"))(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJsonWithInvalidDates)
    }

    "return claimant claims successfully where dates are formatted yyyyMMdd" in new mocks {
      val matchedClaims = Json.parse(claimsJsonWithDatesFormattedYYYYMMDD).as[Claims]
      when(mockNtcConnector.claimantClaims(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]()))
        .thenReturn(Future(matchedClaims))
      stubAuthorisationGrantAccess(Some(nino) and L200)
      override val mockLivePersonalIncomeService: TestPersonalIncomeService = new TestPersonalIncomeService(mockNtcConnector, mockTaiConnector, mockPersonalTaxSummaryConnector, mockTaxCreditsBrokerConnector, mockAuditConnector)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result = await(controller.claimantDetails(Nino(nino), None, Some("claims"))(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJsonWithAllDates)
    }

    "return claimant claims successfully where dates are formatted yyyy-MM-dd" in new mocks {
      val matchedClaims = Json.parse(claimsJsonWithDatesFormattedHyphenatedYYYYMMDD).as[Claims]
      when(mockNtcConnector.claimantClaims(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]()))
        .thenReturn(Future(matchedClaims))
      stubAuthorisationGrantAccess(Some(nino) and L200)
      override val mockLivePersonalIncomeService: TestPersonalIncomeService = new TestPersonalIncomeService(mockNtcConnector, mockTaiConnector, mockPersonalTaxSummaryConnector, mockTaxCreditsBrokerConnector, mockAuditConnector)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result = await(controller.claimantDetails(Nino(nino), None, Some("claims"))(fakeRequest))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJsonWithAllDates)
    }

    "return 404 when no claims matched the supplied nino" in new mocks {
      val matchedClaims = buildClaims(Json.toJson(Json.parse(claimsJson)).as[Claims])
      when(mockNtcConnector.claimantClaims(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]()))
        .thenReturn(Future(matchedClaims))
      stubAuthorisationGrantAccess(Some(nino) and L200)
      override val mockLivePersonalIncomeService: TestPersonalIncomeService = new TestPersonalIncomeService(mockNtcConnector, mockTaiConnector, mockPersonalTaxSummaryConnector, mockTaxCreditsBrokerConnector, mockAuditConnector)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      status(await(controller.claimantDetails(Nino(nino), None, Some("claims"))(fakeRequest))) shouldBe 404
    }

    "return 403 when no tcrAuthHeader is supplied to claimant details API" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino), None, None)(fakeRequest))
      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.parse("""{"code":"NTC_RENEWAL_AUTH_ERROR","message":"No auth header supplied in http request"}""")
    }

    "return 403 when tcrAuthHeader is supplied to claims API" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino), None, Some("claims"))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference, Nino(nino))))
      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.parse("""{"code":"NTC_RENEWAL_AUTH_ERROR","message":"Auth header is not required in the request"}""")
    }

    "return 401 when the nino in the request does not match the authority nino" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      status(await(controller.claimantDetails(incorrectNino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference, incorrectNino)))) shouldBe 401
    }

    "return the claimant details successfully when journeyId is supplied" in new mocks {
      val claimantDetails = ClaimantDetails(false, 1, "r", nino, None, false, "some-app-id")
      stubAuthorisationGrantAccess(Some(nino) and L200)
      stubClaimantDetailsResponse(claimantDetails)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      val result = await(controller.claimantDetails(Nino(nino), Some("unique-journey-id"))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference, Nino(nino))))
      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimantDetails.copy(mainApplicantNino = "true"))
    }

    "return unauthorized when authority record does not contain a NINO" in new mocks {
      stubAuthorisationGrantAccess(None and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference, Nino(nino))))

      status(result) shouldBe 401
      contentAsJson(result) shouldBe Json.parse("""{"code":"UNAUTHORIZED","message":"NINO does not exist on account"}""")
    }

    "return unauthorized when authority record has a low CL" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L50)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference, Nino(nino))))
      status(result) shouldBe 401
      contentAsJson(result) shouldBe Json.parse("""{"code":"LOW_CONFIDENCE_LEVEL","message":"Confidence Level on account does not allow access"}""")
    }

    "return 403 response when the tcr auth header is not supplied in the request" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino))(fakeRequest))
      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.toJson(ErrorNoAuthToken)
    }

    "return status code 406 when the Accept header is invalid" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)
      val requestInvalidHeaders: FakeRequest[AnyContentAsEmpty.type] = FakeRequest().withSession(
        "AuthToken" -> "Some Header"
      ).withHeaders(
        "Authorization" -> "Some Header"
      )
      val controller = new LivePersonalIncomeController(mockAuthConnector, 200)
      status(await(controller.claimantDetails(Nino(nino))(requestInvalidHeaders))) shouldBe 406
    }
  }

  "claimant details Sandbox" should {

    "return claimant details successfully when an unknown bar code reference is supplied" in new mocks {
      val claimantDetails = ClaimantDetails(false, 1, "r", nino, None, false, "some-app-id")
      val controller = new SandboxPersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino))(emptyRequestWithAcceptHeaderAndAuthHeader(RenewalReference("888888888888888"), Nino(nino))))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(claimantDetails.copy(mainApplicantNino = "false", hasPartner = false, renewalFormType = "r"))
    }

    "return claimant claims successfully" in new mocks {
      val claimantDetails = ClaimantDetails(false, 1, "r", nino, None, false, "some-app-id")
      val controller = new SandboxPersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino), None, Some("claims"))(fakeRequest))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.parse(matchedClaimsJson)
    }

    "return claimant details successfully when a known bar code reference is supplied" in new mocks {
      val claimantDetails = ClaimantDetails(false, 1, "r", nino, None, false, "some-app-id")
      case class TestData(barcode: String, renewalFormType: String, hasPartner: Boolean = false)

      val testData = Seq(TestData("111111111111111", "r"), TestData("222222222222222", "d"), TestData("333333333333333", "d2"), TestData("444444444444444", "d", hasPartner = true), TestData("555555555555555", "d2", true))
      val controller = new SandboxPersonalIncomeController(mockAuthConnector, 200)
      testData.map(item => {
        val result = await(controller.claimantDetails(Nino(nino))(emptyRequestWithAcceptHeaderAndAuthHeader(RenewalReference(item.barcode), Nino(nino))))
        status(result) shouldBe 200
        contentAsJson(result) shouldBe Json.toJson(claimantDetails.copy(mainApplicantNino = "true", hasPartner = item.hasPartner, renewalFormType = item.renewalFormType))
      })
    }

    "return 403 response when the tcr auth header is not supplied in the request" in new mocks {
      val controller = new SandboxPersonalIncomeController(mockAuthConnector, 200)
      val result = await(controller.claimantDetails(Nino(nino))(fakeRequest))
      status(result) shouldBe 403
      contentAsJson(result) shouldBe Json.toJson(ErrorNoAuthToken)
    }

    "return status code 406 when the Accept header is invalid" in new mocks {
      val controller = new SandboxPersonalIncomeController(mockAuthConnector, 200)
      val requestInvalidHeaders: FakeRequest[AnyContentAsEmpty.type] = FakeRequest().withSession(
        "AuthToken" -> "Some Header"
      ).withHeaders(
        "Authorization" -> "Some Header"
      )
      val result = await(controller.claimantDetails(Nino(nino))(requestInvalidHeaders))

      status(result) shouldBe 406
    }
  }
}

class TestPersonalIncomeRenewalSpec extends TestSetup with WithFakeApplication {

  val incomeDetails = IncomeDetails(Some(10), Some(20), Some(30), Some(40), Some(true))
  val certainBenefits = CertainBenefits(false, false, false, false, false)
  val otherIncome = OtherIncome(Some(100), Some(false))
  val renewal = TcrRenewal(RenewalData(Some(incomeDetails), Some(incomeDetails),
                Some(certainBenefits)), None, Some(otherIncome), Some(otherIncome), false)

  "renewal Live" should {

    "process the renewal successfully if renewals are enabled" in new mocks {
      stubAuthorisationGrantAccess(Some(nino) and L200)

//      stubTcrRenewal()
      override val mockLivePersonalIncomeService = new TestPersonalIncomeService(mockNtcConnector, mockTaiConnector,
        mockPersonalTaxSummaryConnector, mockTaxCreditsBrokerConnector, mockAuditConnector)
      val controller = new TestPersonalIncomeController(mockAuthConnector, mockLivePersonalIncomeService, 200)
      status(await(controller.submitRenewal(Nino(nino)).apply(fakeRequest.withBody()))) shouldBe 200
      verify(mockNtcConnector.submitRenewal(any[TaxCreditsNino](), any[TcrRenewal]()), times(1))
    }

  }


}
//
//  "renewal Live" should {
//
//    "process the renewal successfully if renewals are enabled" in new Success {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))
//
//      ntcConnector.renewalCount shouldBe 1
//      status(result) shouldBe 200
//    }
//
//    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
//      val result = await(controller.submitRenewal(ninoIncorrect)(jsonRenewalRequestWithAuthHeader))
//
//      status(result) shouldBe 401
//    }
//
//    "process the renewal successfully if renewals are disabled" in new SuccessRenewalDisabled {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))
//
//      ntcConnector.renewalCount shouldBe 0
//      status(result) shouldBe 200
//    }
//
//    "process returns a 200 successfully when journeyId is supplied" in new Success {
//      val result = await(controller.submitRenewal(nino, Some(journeyId))(jsonRenewalRequestWithAuthHeader))
//
//      ntcConnector.renewalCount shouldBe 1
//      status(result) shouldBe 200
//    }
//
//    "process returns a 200 when journeyId is supplied and Renewals are disabled" in new SuccessRenewalDisabled {
//      val result = await(controller.submitRenewal(nino, Some(journeyId))(jsonRenewalRequestWithAuthHeader))
//
//      ntcConnector.renewalCount shouldBe 0
//      status(result) shouldBe 200
//    }
//
//    "return 403 result when no tcr auth header has been supplied" in new Success {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithNoAuthHeader))
//
//      status(result) shouldBe 403
//    }
//
//    "return bad result request when invalid json is submitted" in new Success {
//      val result = await(controller.submitRenewal(nino)(renewalBadRequest))
//
//      status(result) shouldBe 400
//    }
//
//    "Return 401 result when authority record does not contain a NINO" in new AuthWithoutNino {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))
//
//      status(result) shouldBe 401
//      contentAsJson(result) shouldBe noNinoOnAccont
//
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//    "return unauthorized when authority record has a low CL" in new AuthWithLowCL {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))
//
//      status(result) shouldBe 401
//      contentAsJson(result) shouldBe lowCl
//
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//    "return 406 result when the headers are invalid" in new Success {
//      val result: Result = await(controller.submitRenewal(nino)(jsonRenewalRequestNoAcceptHeader))
//
//      status(result) shouldBe 406
//    }
//  }
//
//  "renewal sandbox" should {
//
//    "return success response" in new SandboxSuccess {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithAuthHeader))
//
//      status(result) shouldBe 200
//    }
//
//    "return 403 result when no tcr auth header has been supplied" in new Success {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestWithNoAuthHeader))
//
//      status(result) shouldBe 403
//    }
//
//    "return 406 result when the headers are invalid" in new SandboxSuccess {
//      val result = await(controller.submitRenewal(nino)(jsonRenewalRequestNoAcceptHeader))
//
//      status(result) shouldBe 406
//    }
//  }
//}
//
//
//class TestPersonalIncomeRenewalSummarySpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {
//
//  override lazy val fakeApplication = FakeApplication(additionalConfiguration = config)
//
//  "tax credits summary live" should {
//
//    "process the request successfully and filter children older than 20 and where deceased flags are active" in new Success {
//      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 200
//      contentAsJson(result) shouldBe Json.toJson(taxRenewalSummaryWithoutChildrenOverAge20)
//      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
//    }
//
//    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
//      val result = await(controller.taxCreditsSummary(ninoIncorrect)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 401
//    }
//
//    "return 429 HTTP status when retrieval of children returns 503" in new Generate_503 {
//      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 429
//      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
//    }
//
//    "return the summary successfully when journeyId is supplied" in new Success {
//      val result = await(controller.taxCreditsSummary(nino, Some(journeyId))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 200
//      contentAsJson(result) shouldBe Json.toJson(taxRenewalSummaryWithoutChildrenOverAge20)
//      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
//    }
//
//    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
//      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeader))
//
//      status(result) shouldBe 401
//      contentAsJson(result) shouldBe noNinoOnAccont
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//    "return unauthorized when authority record has a low CL" in new AuthWithLowCL {
//      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeader))
//
//      status(result) shouldBe 401
//      contentAsJson(result) shouldBe lowCl
//
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//    "return status code 406 when the headers are invalid" in new Success {
//      val result = await(controller.taxCreditsSummary(nino)(emptyRequest))
//
//      status(result) shouldBe 406
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//  }
//
//  "tax credits summary Sandbox" should {
//
//    "return the summary response from a resource" in new SandboxSuccess {
//      val result = await(controller.taxCreditsSummary(nino)(emptyRequestWithAcceptHeader))
//
//      status(result) shouldBe 200
//
//      val resource = findResource(s"/resources/taxcreditsummary/${nino.value}.json")
//      contentAsJson(result) shouldBe Json.parse(resource.get)
//
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//  }
//}
//
//
//class TestExclusionsServiceSpec extends UnitSpec with WithFakeApplication with ScalaFutures with StubApplicationConfiguration {
//
//  "tax exclusions service" should {
//
//    "process the request for get tax credit exclusion successfully" in new Success {
//      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 200
//      contentAsJson(result) shouldBe exclusionResult
//      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
//    }
//
//    "return 401 when the nino in the request does not match the authority nino" in new AccessCheck {
//      val result = await(controller.getTaxCreditExclusion(ninoIncorrect)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 401
//    }
//
//    "return 429 HTTP status when get tax credit exclusion returns 503" in new Generate_503 {
//      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 429
//      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
//    }
//
//    "return the tax credit exclusion successfully when journeyId is supplied" in new Success {
//      val result = await(controller.getTaxCreditExclusion(nino, Some(journeyId))(emptyRequestWithAcceptHeaderAndAuthHeader(renewalReference)))
//
//      status(result) shouldBe 200
//      contentAsJson(result) shouldBe exclusionResult
//      testPersonalIncomeService.saveDetails shouldBe Map("nino" -> nino.value)
//    }
//
//    "return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
//      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeader))
//
//      status(result) shouldBe 401
//      contentAsJson(result) shouldBe noNinoOnAccont
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//    "return unauthorized when authority record has a low Confidence Level" in new AuthWithLowCL {
//      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequestWithAcceptHeader))
//
//      status(result) shouldBe 401
//      contentAsJson(result) shouldBe lowCl
//
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//
//    "return status code 406 when the headers are invalid" in new Success {
//      val result = await(controller.getTaxCreditExclusion(nino)(emptyRequest))
//
//      status(result) shouldBe 406
//      testPersonalIncomeService.saveDetails shouldBe Map.empty
//    }
//  }
//}

