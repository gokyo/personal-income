package uk.gov.hmrc.personalincome.stubs

import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, get, stubFor, urlPathEqualTo}
import uk.gov.hmrc.domain.Nino

object TaxCreditsBrokerStub {
  val childrenJson =
    """{ "child": [
        {
          "firstNames": "Sarah",
          "surname": "Smith",
          "dateOfBirth": 936057600000,
          "hasFTNAE": false,
          "hasConnexions": false,
          "isActive": true
        },
        {
          "firstNames": "Joseph",
          "surname": "Smith",
          "dateOfBirth": 884304000000,
          "hasFTNAE": false,
          "hasConnexions": false,
          "isActive": true
        },
        {
          "firstNames": "Mary",
          "surname": "Smith",
          "dateOfBirth": 852768000000,
          "hasFTNAE": false,
          "hasConnexions": false,
          "isActive": true
        } ] }"""

  def childrenAreFound(nino: Nino): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${nino.value}/children")).willReturn(
      aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(childrenJson)))

  def partnerJson(nino:Nino) =
    s"""{
        "forename": "Frederick",
        "otherForenames": "Tarquin",
        "surname": "Hunter-Smith",
        "nino": "${nino.value}",
        "address": {
          "addressLine1": "999 Big Street",
          "addressLine2": "Worthing",
          "addressLine3": "West Sussex",
          "postCode": "BN99 8IG"
        }
      }""".stripMargin

  def partnerDetailsAreFound(climantsNino: Nino, partnersNino:Nino): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${climantsNino.value}/partner-details")).willReturn(
      aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(partnerJson(partnersNino))))

  val paymentSummaryJson =
    """
      |{
      |    "workingTaxCredit": {
      |      "paymentSeq": [
      |        {
      |          "amount": 55,
      |          "paymentDate": 1509008158781,
      |          "oneOffPayment": false,
      |          "earlyPayment": false
      |        },
      |        {
      |          "amount": 55,
      |          "paymentDate": 1511690158781,
      |          "oneOffPayment": false,
      |          "holidayType": "bankHoliday",
      |          "earlyPayment": true,
      |          "explanatoryText" : "Your payment is early because of UK bank holidays."
      |        },
      |        {
      |          "amount": 55,
      |          "paymentDate": 1514282158781,
      |          "oneOffPayment": true,
      |          "earlyPayment": false,
      |          "explanatoryText" : "This is because of a recent change and is to help you get the right amount of tax credits."
      |        }
      |      ],
      |      "paymentFrequency": "weekly",
      |      "previousPaymentSeq": [
      |        {
      |          "amount": 33,
      |          "paymentDate": 1503737758781,
      |          "oneOffPayment": false,
      |          "earlyPayment": false
      |        },
      |        {
      |          "amount": 43,
      |          "paymentDate": 1501059358781,
      |          "oneOffPayment": false,
      |          "holidayType": "bankHoliday",
      |          "earlyPayment": true,
      |          "explanatoryText" : "Your payment was early because of UK bank holidays."
      |        },
      |        {
      |          "amount": 53,
      |          "paymentDate": 1498467358781,
      |          "oneOffPayment": true,
      |          "earlyPayment": false,
      |          "explanatoryText" : "This was because of a recent change and was to help you get the right amount of tax credits."
      |        }
      |      ]
      |    },
      |    "childTaxCredit": {
      |      "paymentSeq": [
      |        {
      |          "amount": 55,
      |          "paymentDate": 1509008158781,
      |          "oneOffPayment": false,
      |          "earlyPayment": false
      |        },
      |        {
      |          "amount": 55,
      |          "paymentDate": 1511690158781,
      |          "oneOffPayment": false,
      |          "holidayType": "bankHoliday",
      |          "earlyPayment": true,
      |          "explanatoryText" : "Your payment is early because of UK bank holidays."
      |        },
      |        {
      |          "amount": 55,
      |          "paymentDate": 1514282158781,
      |          "oneOffPayment": true,
      |          "earlyPayment": false,
      |          "explanatoryText" : "This is because of a recent change and is to help you get the right amount of tax credits."
      |        }
      |      ],
      |      "paymentFrequency": "weekly"
      |    },
      |    "paymentEnabled": true,
      |    "totalsByDate": [
      |      {
      |        "amount": 110,
      |        "paymentDate": 1509008158781
      |      },
      |      {
      |        "amount": 110,
      |        "paymentDate": 1511690158781
      |      },
      |      {
      |        "amount": 110,
      |        "paymentDate": 1514282158781
      |      }
      |    ],
      |    "previousTotalsByDate": [
      |      {
      |        "amount": 53,
      |        "paymentDate": 1498467358781
      |      },
      |      {
      |        "amount": 43,
      |        "paymentDate": 1501059358781
      |      },
      |      {
      |        "amount": 33,
      |        "paymentDate": 1503737758781
      |      }
      |    ]
      |  }
    """.stripMargin

  def paymntSummaryIsFound(nino: Nino): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${nino.value}/payment-summary")).willReturn(
      aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(paymentSummaryJson)))

  def personalDetailsJson(nino: Nino) =
    s"""
      {
          "forename": "Nuala",
          "surname": "O'Shea",
          "nino": "${nino.value}",
          "address": {
            "addressLine1": "999 Big Street",
            "addressLine2": "Worthing",
            "addressLine3": "West Sussex",
            "postCode": "BN99 8IG"
         }
        }
    """

  def personalDetailsAreFound(nino: Nino): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${nino.value}/personal-details")).willReturn(
      aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(personalDetailsJson(nino))))

  def exlusionFlagIsFound(nino: Nino, excluded:Boolean): Unit =
    stubFor(get(urlPathEqualTo(s"/tcs/${nino.value}/exclusion")).willReturn(
      aResponse().withStatus(200).withHeader("Content-Type", "application/json").withBody(s"""{ "excluded" : $excluded }""")))
}
