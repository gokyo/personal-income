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

package uk.gov.hmrc.personalincome.connectors

import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.domain.{Nino, SaUtr}
import uk.gov.hmrc.personalincome.domain.CredentialStrength
import uk.gov.hmrc.play.auth.microservice.connectors.ConfidenceLevel
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpGet, HttpResponse}
import uk.gov.hmrc.play.test.UnitSpec
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future

class AuthConnectorSpec extends UnitSpec with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val hc = HeaderCarrier()

  def authConnector(response : HttpResponse, cl: ConfidenceLevel = ConfidenceLevel.L200) = new AuthConnector {

    override def http: HttpGet = new HttpGet {
      override protected def doGet(url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = Future.successful(response)

      override val hooks: Seq[HttpHook] = Seq.empty
    }

    override val serviceUrl: String = "http://localhost"

    override def serviceConfidenceLevel: ConfidenceLevel = cl
  }

  "Returning the accounts" should {

    "be found and routeToIV and routeToTwoFactor should be true" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L50
      val credentialStrength = CredentialStrength.Weak

      val saUtr = Some(SaUtr("1872796160"))
      val nino = Some(Nino("CS100700A"))
      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      val accounts = await(authConnector(response, serviceConfidenceLevel).accounts())

      accounts.nino.get shouldBe nino.get
      accounts.saUtr.get shouldBe saUtr.get
      accounts.routeToIV shouldBe true
      accounts.routeToTwoFactor shouldBe true
    }

    "be found and routeToIV is false and routeToTwoFactor is true" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L50
      val credentialStrength = CredentialStrength.Strong

      val saUtr = Some(SaUtr("1872796160"))
      val nino = Some(Nino("CS100700A"))
      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      val accounts = await(authConnector(response, serviceConfidenceLevel).accounts())

      accounts.nino.get shouldBe nino.get
      accounts.saUtr.get shouldBe saUtr.get
      accounts.routeToIV shouldBe true
      accounts.routeToTwoFactor shouldBe false
    }

    "be found and routeToIV and routeToTwoFactor should be false" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L200
      val credentialStrength = CredentialStrength.Strong

      val saUtr = Some(SaUtr("1872796160"))
      val nino = Some(Nino("CS100700A"))
      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      val accounts = await(authConnector(response, serviceConfidenceLevel).accounts())

      accounts.nino.get shouldBe nino.get
      accounts.saUtr.get shouldBe saUtr.get
      accounts.routeToIV shouldBe false
      accounts.routeToTwoFactor shouldBe false
    }

    "be found when the users account does not have a NINO" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L200
      val credentialStrength = CredentialStrength.Strong

      val saUtr = Some(SaUtr("1872796160"))
      val nino = None
      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      val accounts = await(authConnector(response, serviceConfidenceLevel).accounts())

      accounts.nino shouldBe None
      accounts.saUtr.get shouldBe saUtr.get
      accounts.routeToIV shouldBe false
      accounts.routeToTwoFactor shouldBe false
    }

  }

  "grantAccess" should {

    "error with unauthorised when account has low CL" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L50
      val credentialStrength = CredentialStrength.Weak
      val saUtr = Some(SaUtr("1872796160"))
      val nino = None

      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      try {
        await(authConnector(response, serviceConfidenceLevel).grantAccess(nino))
      } catch {
        case e: AccountWithLowCL =>
          e.message shouldBe "The user does not have sufficient CL permissions to access this service"
        case t: Throwable =>
          fail("Unexpected error failure")
      }
    }

    "fail to find Nino only accounts when credStrength is weak" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L200
      val credentialStrength = CredentialStrength.Weak
      val saUtr = None
      val nino = Some(Nino("CS100700A"))

      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      try {
        await(authConnector(response, serviceConfidenceLevel).grantAccess(nino))
      } catch {
        case e: AccountWithWeakCredStrength =>
          e.message shouldBe "The user does not have sufficient credential strength permissions to access this service"
        case t: Throwable =>
          fail("Unexpected error failure with exception " + t)
      }
    }

    "successfully return account with NINO when SAUTR is empty" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L200
      val credentialStrength = CredentialStrength.Strong

      val saUtr = Some(SaUtr("1872796160"))
      val nino = Some(Nino("CS100700A"))
      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      await(authConnector(response, serviceConfidenceLevel).grantAccess(nino))
    }

    "find NINO only account when credStrength and CL are correct" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L200
      val credentialStrength = CredentialStrength.Strong
      val saUtr = None
      val nino = Some(Nino("CS100700A"))

      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      await(authConnector(response, serviceConfidenceLevel).grantAccess(nino))

    }

    "fail to return authority when no NINO exists" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L200
      val credentialStrength = CredentialStrength.Strong
      val saUtr = None
      val nino = None

      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, nino)))

      try {
        await(authConnector(response, serviceConfidenceLevel).grantAccess(nino))
      } catch {
        case e: NinoNotFoundOnAccount =>
          e.message shouldBe "The user must have a National Insurance Number"
        case t: Throwable =>
          fail("Unexpected error failure with exception " + t)
      }
    }

    "fail to return authority when auth NINO does not match request NINO" in {

      val serviceConfidenceLevel = ConfidenceLevel.L200
      val authorityConfidenceLevel = ConfidenceLevel.L200
      val credentialStrength = CredentialStrength.Strong
      val saUtr = None
      val ninoAuth = Some(Nino("CS100700A"))
      val ninoRequest = Some(Nino("CS333700A"))

      val response = HttpResponse(200, Some(authorityJson(authorityConfidenceLevel, credentialStrength, saUtr, ninoAuth)))

      try {
        await(authConnector(response, serviceConfidenceLevel).grantAccess(ninoRequest))
      } catch {
        case e: FailToMatchTaxIdOnAuth =>
          e.message shouldBe "The nino in the URL failed to match auth!"
        case t: Throwable =>
          fail("Unexpected error failure with exception " + t)
      }
    }


  }

  def authorityJson(confidenceLevel: ConfidenceLevel, credStrength:CredentialStrength, saUtr: Option[SaUtr], nino : Option[Nino]): JsValue = {

    val sa : String = saUtr match {
      case Some(utr) => s"""
                          | "sa": {
                          |            "link": "/sa/individual/$utr",
                          |            "utr": "$utr"
                          |        },
                        """.stripMargin
      case None => ""
    }

    val paye = nino match {
      case Some(n) => s"""
                          "paye": {
                          |            "link": "/paye/individual/$n",
                          |            "nino": "$n"
                          |        },
                        """.stripMargin
      case None => ""
    }

    val json =
      s"""
         |{
         |    "accounts": {
         |       $sa
         |       $paye
         |        "ct": {
         |            "link": "/ct/8040200779",
         |            "utr": "8040200779"
         |        },
         |        "vat": {
         |            "link": "/vat/999904829",
         |            "vrn": "999904829"
         |        },
         |        "epaye": {
         |            "link": "/epaye/754%2FMODES02",
         |            "empRef": "754/MODES02"
         |        }
         |    },
         |    "confidenceLevel": ${confidenceLevel.level},
         |    "credentialStrength": "${credStrength.name}"
         |}
      """.stripMargin

    Json.parse(json)
  }
}
