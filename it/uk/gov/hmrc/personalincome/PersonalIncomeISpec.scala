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

package uk.gov.hmrc.personalincome

import play.api.libs.json.JsArray
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.stubs.AuthStub.grantAccess
import uk.gov.hmrc.personalincome.stubs.NtcStub._
import uk.gov.hmrc.personalincome.stubs.PersonalTaxSummaryStub._
import uk.gov.hmrc.personalincome.stubs.TaiStub.taxSummaryExists
import uk.gov.hmrc.personalincome.stubs.TaxCreditsBrokerStub._
import uk.gov.hmrc.personalincome.support.BaseISpec

class PersonalIncomeISpec extends BaseISpec {
  "GET /income/:nino/tax-summary/:year" should {
    val year = 2017
    val url = wsUrl(s"/income/${nino1.value}/tax-summary/$year").withHeaders(acceptJsonHeader)

    "return a tax summary including additions and reductions" in {
      grantAccess(nino1.value)
      taxSummaryExists(nino1, year)
      estimatedIncomeExists(nino1)
      yourTaxableIncomeExists(nino1)

      val response = await(url.get())

      withClue(response.body) {
        response.status shouldBe 200
      }

      val estimatedIncome = response.json \ "estimatedIncomeWrapper" \ "estimatedIncome"

      val additionalTaxTable = (estimatedIncome \ "additionalTaxTable").as[JsArray]
      (additionalTaxTable(0) \ "description").as[String] shouldBe "Child Benefit"
      (additionalTaxTable(0) \ "amount").as[BigDecimal] shouldBe BigDecimal("1500.99")

      (additionalTaxTable(1) \ "description").as[String] shouldBe "Estimate of the tax you owe this year"
      (additionalTaxTable(1) \ "amount").as[BigDecimal] shouldBe BigDecimal(500)

      (estimatedIncome \ "additionalTaxTableTotal").as[BigDecimal] shouldBe BigDecimal("2000.99")

      val reductionsTable = (estimatedIncome \ "reductionsTable").as[JsArray]
      (reductionsTable(1) \ "description").as[String] shouldBe "Tax on dividends"
      (reductionsTable(1) \ "amount").as[BigDecimal] shouldBe BigDecimal(-2000)
      (reductionsTable(1) \ "additionalInfo").as[String] shouldBe
        "Interest from company dividends is taxed at the dividend ordinary rate (10%) before it is paid to you."

      (estimatedIncome \ "reductionsTableTotal").as[BigDecimal] shouldBe BigDecimal(-3040)
    }

    "return 404 when no tax summary is found " in {
      grantAccess(nino1.value)
      yourTaxableIncomeIsNotFound(nino1)

      val response = await(url.get())

      response.status shouldBe 404
    }

    "return 500 when personal-tax-summary returns an unparseable amount" in {
      grantAccess(nino1.value)
      taxSummaryExists(nino1, year)
      estimatedIncomeExistsWithUnparseableAmount(nino1)
      yourTaxableIncomeExists(nino1)

      val response = await(url.get())

      response.status shouldBe 500
    }
  }

  "GET /income/:nino/tax-credits/:renewalReference/auth" should {
    val url = wsUrl(s"/income/${nino1.value}/tax-credits/${renewalReference.value}/auth").withHeaders(acceptJsonHeader)

    "return a tcrAuthenticationToken" in {

      grantAccess(nino1.value)
      authenticationRenewalSuccessful(nino1,renewalReference,tcrAuthenticationToken)

      val response = await(url.get())

      response.status shouldBe 200
      (response.json \ "tcrAuthToken").as[String] shouldBe tcrAuthenticationToken
    }

    "return 404 when no auth tcrAuthenticationToken is found " in {
      grantAccess(nino1.value)
      authenticationRenewalNotFound(nino1,renewalReference)

      val response = await(url.get())

      response.status shouldBe 404
    }
  }

  "GET /income/:nino/tax-credits/claimant-details" should {
    def request(nino:Nino) = wsUrl(s"/income/${nino.value}/tax-credits/claimant-details").withHeaders(acceptJsonHeader, tcrAuthTokenHeader)

    "retrieve claimant details for main applicant" in {
      grantAccess(nino1.value)
      claimantDetailsAreFoundFor(nino1, nino1, nino2)

      val response = await(request(nino1).get())

      response.status shouldBe 200
      (response.json \ "mainApplicantNino").as[String] shouldBe "true"
    }

    "retrieve claimant details for partner" in {
      grantAccess(nino2.value)
      claimantDetailsAreFoundFor(nino2, nino1, nino2)

      val response = await(request(nino2).get())

      response.status shouldBe 200
      (response.json \ "mainApplicantNino").as[String] shouldBe "false"
    }

    "return 404 when claimant details are not found" in {
      grantAccess(nino1.value)
      claimantDetailsAreNotFoundFor(nino1)

      val response = await(request(nino1).get())

      response.status shouldBe 404
    }
  }

  "GET /income/:nino/tax-credits/tax-credits-summary " should {
    def request(nino:Nino) = wsUrl(s"/income/${nino.value}/tax-credits/tax-credits-summary").withHeaders(acceptJsonHeader, tcrAuthTokenHeader)

    "return a tax credit summary " in {
      grantAccess(nino1.value)
      childrenAreFound(nino1)
      partnerDetailsAreFound(nino1,nino2)
      paymntSummaryIsFound(nino1)
      personalDetailsAreFound(nino1)

      val response = await(request(nino1).get())
      response.status shouldBe 200
      (response.json \ "showData").as[Boolean] shouldBe false
    }
  }

  "GET /income/:nino/tax-credits/tax-credits-decision" should {
    def request(nino:Nino) = wsUrl(s"/income/${nino.value}/tax-credits/tax-credits-decision").withHeaders(acceptJsonHeader, tcrAuthTokenHeader)

    "return showData == false if excluded" in {
      grantAccess(nino1.value)
      exlusionFlagIsFound(nino1, excluded=true)

      val response = await(request(nino1).get())
      response.status shouldBe 200
      (response.json \ "showData").as[Boolean] shouldBe false
    }

    "return showData == true if not excluded" in {
      grantAccess(nino1.value)
      exlusionFlagIsFound(nino1, excluded=false)

      val response = await(request(nino1).get())
      response.status shouldBe 200
      (response.json \ "showData").as[Boolean] shouldBe true
    }
  }
}
