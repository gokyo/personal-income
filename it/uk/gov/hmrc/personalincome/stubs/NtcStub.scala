package uk.gov.hmrc.personalincome.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import play.api.libs.json.Json.toJson
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.domain.{RenewalReference, TcrRenewal}

object NtcStub {
  def multipleClaimsJson(applicant1Nino:Nino, applicant2Nino:Nino = Nino("AP412713B")) =  s"""{
       |  "references": [
       |    {
       |      "household": {
       |        "barcodeReference": "200000000000012",
       |        "applicationID": "198765432134566",
       |        "applicant1": {
       |          "nino": "${applicant1Nino.value}",
       |          "title": "Miss",
       |          "firstForename": "Emma",
       |          "secondForename": "",
       |          "surname": "Cowling"
       |        }
       |      },
       |      "renewal": {
       |        "awardStartDate": "2016-04-05",
       |        "awardEndDate": "2016-08-31",
       |        "renewalNoticeIssuedDate": "20301012",
       |        "renewalNoticeFirstSpecifiedDate": "20101012"
       |      }
       |    },
       |    {
       |      "household": {
       |        "barcodeReference": "200000000000013",
       |        "applicationID": "198765432134567",
       |        "applicant1": {
       |          "nino": "${applicant1Nino.value}",
       |          "title": "Miss",
       |          "firstForename": "Emma",
       |          "secondForename": "",
       |          "surname": "Cowling"
       |        }
       |      },
       |      "renewal": {
       |        "awardStartDate": "2016-08-31",
       |        "awardEndDate": "2016-12-31",
       |        "renewalNoticeIssuedDate": "20301012",
       |        "renewalNoticeFirstSpecifiedDate": "20101012"
       |      }
       |    },
       |    {
       |      "household": {
       |        "barcodeReference": "200000000000014",
       |        "applicationID": "198765432134568",
       |        "applicant1": {
       |          "nino": "${applicant1Nino.value}",
       |          "title": "Miss",
       |          "firstForename": "Hazel",
       |          "secondForename": "",
       |          "surname": "Young"
       |        },
       |        "applicant2": {
       |          "nino": "${applicant2Nino.value}",
       |          "title": "Miss",
       |          "firstForename": "Cathy",
       |          "secondForename": "",
       |          "surname": "Garcia-Vazquez"
       |        }
       |      },
       |      "renewal": {
       |        "awardStartDate": "2016-12-31",
       |        "awardEndDate": "2017-07-31",
       |        "renewalNoticeIssuedDate": "20301012",
       |        "renewalNoticeFirstSpecifiedDate": "20101012"
       |      }
       |    }
       |  ]
       |}""".stripMargin


  def authenticationRenewalNotFound(nino: Nino, renewalReference: RenewalReference): Unit =
    stubFor(get(urlPathEqualTo(
      s"/tcs/${nino.value}/${renewalReference.value}/auth")).willReturn(aResponse().withStatus(404)))

  def authenticationRenewalSuccessful(nino: Nino, renewalReference: RenewalReference, token:String): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${nino.value}/${renewalReference.value}/auth")).willReturn(
      aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(
        s"""{ "tcrAuthToken" : "$token" }""")))

  def claimantDetailsAreFoundFor(currentUserNino: Nino, mainApplicant1Nino: Nino, applicant2Nino: Nino): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${currentUserNino.value}/claimant-details")).willReturn(
      aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(
        s"""{
           | "hasPartner": true,
           | "claimantNumber" : 123,
           | "renewalFormType": "renewalForm",
           | "mainApplicantNino" : "${mainApplicant1Nino.value}",
           | "usersPartnerNino" : "${applicant2Nino.value}",
           | "availableForCOCAutomation" : false,
           | "applicationId" : "some-app-id" }""".stripMargin
      )))

  def claimantDetailsAreNotFoundFor(nino: Nino): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${nino.value}/claimant-details")).willReturn(aResponse().withStatus(404)))

  def renewalIsSuccessful(nino: Nino, renewalData: TcrRenewal): Unit =
    stubFor(post(urlEqualTo(s"/tcs/${nino.value}/renewal")).withRequestBody(
      equalToJson(toJson(renewalData).toString(), true, false)).willReturn(aResponse().withStatus(200)))
}
