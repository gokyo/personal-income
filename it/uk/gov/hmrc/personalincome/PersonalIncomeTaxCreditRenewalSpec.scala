package uk.gov.hmrc.personalincome

import com.github.tomakehurst.wiremock.client.WireMock.{postRequestedFor, urlEqualTo, verify}
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC
import play.api.libs.json.Json.toJson
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.domain.{IncomeDetails, RenewalData, TcrRenewal}
import uk.gov.hmrc.personalincome.stubs.AuthStub.grantAccess
import uk.gov.hmrc.personalincome.stubs.NtcStub.renewalIsSuccessful
import uk.gov.hmrc.personalincome.support.BaseISpec
import uk.gov.hmrc.time.DateTimeUtils

class PersonalIncomeTaxCreditRenewalSpec extends BaseISpec{
  protected val incomeDetails = IncomeDetails(Some(10), Some(20), Some(30), Some(40), Some(true))
  protected val renewal = TcrRenewal(RenewalData(Some(incomeDetails), None, None), None, None, None, false)

  protected val now: DateTime = DateTimeUtils.now.withZone(UTC)

  protected val submissionStateEnabledRequest = wsUrl(s"/income/tax-credits/submission/state/enabled").withHeaders(acceptJsonHeader)

  protected def submissionShuttered = false
  protected def submissionStartDate = now.minusDays(1).toString
  protected def submissionEndDate = now.plusDays(1).toString
  protected def endViewRenewalsDate = now.plusDays(2).toString

  override def config = {
     super.config ++
     Map(
       "microservice.services.ntc.submission.submissionShuttered" -> submissionShuttered,
       "microservice.services.ntc.submission.startDate" -> submissionStartDate,
       "microservice.services.ntc.submission.endDate" -> submissionEndDate,
       "microservice.services.ntc.submission.endViewRenewalsDate" -> endViewRenewalsDate)
   }

  protected def submitTaxCreditRenewal = {
    def request(nino: Nino) = wsUrl(s"/income/${nino.value}/tax-credits/renewal").withHeaders(acceptJsonHeader, tcrAuthTokenHeader)

    grantAccess(nino1.value)

    val renewalJson = toJson(renewal)
    val response = await(request(nino1).post(renewalJson))
    response.status shouldBe 200
  }

  protected def verifyNoSubmissionForPostToTaxCreditsRenewlEndpoint = {
    submitTaxCreditRenewal
    verify(0, postRequestedFor(urlEqualTo(s"/tcs/${nino1.value}/renewal")))
  }
}


class TaxCreditRenewalOpenSpec extends PersonalIncomeTaxCreditRenewalSpec{
  "POST /income/:nino/tax-credits/renewal" should {
    "renew when submissions are enabled" in {
      renewalIsSuccessful(nino1, renewal)
      submitTaxCreditRenewal
      verify(1, postRequestedFor(urlEqualTo(s"/tcs/${nino1.value}/renewal")))
    }
  }

  "GET /income/tax-credits/submission/state/enabled" should {
    "return open state " in {
      val response = await(submissionStateEnabledRequest.get)
      response.status shouldBe 200
      (response.json \ "submissionsState").as[String] shouldBe "open"
    }
  }
}


class PersonalIncomeTaxCreditRenewalClosedSpec extends PersonalIncomeTaxCreditRenewalSpec{
  override def submissionStartDate = now.plusDays(1).toString
  override def submissionEndDate = now.plusDays(2).toString
  override def endViewRenewalsDate = now.plusDays(3).toString

  "POST /income/:nino/tax-credits/renewal" should {
    "return OK but not renew when submissions are closed" in {
      verifyNoSubmissionForPostToTaxCreditsRenewlEndpoint
    }
  }

  "GET /income/tax-credits/submission/state/enabled" should {
    "return closed state " in {
      val response = await(submissionStateEnabledRequest.get)
      response.status shouldBe 200
      (response.json \ "submissionsState").as[String] shouldBe "closed"
    }
  }
}


class PersonalIncomeTaxCreditRenewalShutteredSpec extends PersonalIncomeTaxCreditRenewalSpec{
  override def submissionShuttered: Boolean = true

  "POST /income/:nino/tax-credits/renewal" should {
    "return OK but not renew when submissions are shuttered" in {
      verifyNoSubmissionForPostToTaxCreditsRenewlEndpoint
    }
  }

  "GET /income/tax-credits/submission/state/enabled" should {
    "return shuttered state " in {
      val response = await(submissionStateEnabledRequest.get)
      response.status shouldBe 200
      (response.json \ "submissionsState").as[String] shouldBe "shuttered"
    }
  }
}


class PersonalIncomeTaxCreditRenewalCheckStatusOnlyPeriodSpec extends PersonalIncomeTaxCreditRenewalSpec{
  override def submissionStartDate = now.minusDays(2).toString
  override def submissionEndDate = now.minusDays(1).toString
  override def endViewRenewalsDate = now.plusDays(1).toString

  "POST /income/:nino/tax-credits/renewal" should {
    "return OK but not renew when submissions are view-only" in {
      verifyNoSubmissionForPostToTaxCreditsRenewlEndpoint
    }
  }

  "GET /income/tax-credits/submission/state/enabled" should {
    "return check-only state " in {
      val response = await(submissionStateEnabledRequest.get)
      response.status shouldBe 200
      (response.json \ "submissionsState").as[String] shouldBe "check_status_only"
    }
  }
}
