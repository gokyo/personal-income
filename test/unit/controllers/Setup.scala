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

package unit.controllers

import models.{RenewalData, IncomeDetails, TcrRenewal}
import play.api.libs.json.{Json, JsValue}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.apigateway.personalincome.connectors._
import uk.gov.hmrc.apigateway.personalincome.controllers.PersonalIncomeController
import uk.gov.hmrc.apigateway.personalincome.controllers.action.{AccountAccessControlForSandbox, AccountAccessControlWithHeaderCheck, AccountAccessControl}
import uk.gov.hmrc.apigateway.personalincome.domain.{Accounts, TaxCreditsNino, TaxSummaryDetails}
import uk.gov.hmrc.apigateway.personalincome.services.{PersonalIncomeService, SandboxPersonalIncomeService, LivePersonalIncomeService}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.auth.microservice.connectors.ConfidenceLevel
import uk.gov.hmrc.play.http.ws.WSPost
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpPost, HttpGet}

import scala.concurrent.{ExecutionContext, Future}

class TestTaiConnector(taxSummaryDetails:TaxSummaryDetails) extends TaiConnector {
  override def http: HttpGet with HttpPost = ???

  override def serviceUrl: String = ???

  override def taxSummary(nino : Nino, year : Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    Future.successful(taxSummaryDetails)
  }
}

class TestNtcConnector(response:Response) extends NtcConnector {
  override def http: WSPost = ???

  override def serviceUrl: String = ???

  override def submitRenewal(nino: TaxCreditsNino,
                             renewalData: TcrRenewal)(implicit headerCarrier: HeaderCarrier): Future[Response] = {
    Future.successful(response)
  }
}

class TestAuthConnector(nino:Option[Nino]) extends AuthConnector {
  override val serviceUrl: String = "someUrl"

  override def serviceConfidenceLevel: ConfidenceLevel = ???

  override def http: HttpGet = ???

  override def accounts()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Accounts] = Future(Accounts(nino, None))

  override def hasNino()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = Future(Unit)
}

class TestPersonalIncomeService(testTaiConnector:TestTaiConnector, testAuthConnector:TestAuthConnector, testNtcConnector:NtcConnector) extends LivePersonalIncomeService {
  var saveDetails:Map[String, String]=Map.empty

  override val taiConnector = testTaiConnector
  override val authConnector = testAuthConnector
  override val ntcConnector = testNtcConnector

  override def audit(method:String, details:Map[String, String])(implicit hc: HeaderCarrier): Unit = {
    saveDetails=details
  }
}

class TestAccessCheck(testAuthConnector: TestAuthConnector) extends AccountAccessControl {
  override val authConnector: AuthConnector = testAuthConnector
}

class TestAccountAccessControlWithAccept(testAccessCheck:AccountAccessControl) extends AccountAccessControlWithHeaderCheck {
  override val accessControl: AccountAccessControl = testAccessCheck
}


trait Setup {
  implicit val hc = HeaderCarrier()

  val nino = Nino("CS700100A")
  val taxSummaryDetails = TaxSummaryDetails(nino.value,1)
  val incomeDetails = IncomeDetails(Some(10), Some(20), Some(30), Some(40), Some(true))
  val renewal = TcrRenewal(RenewalData(Some(incomeDetails), None, None), None, None, None, false)

  val emptyRequest = FakeRequest()
  val renewalJsonBody: JsValue = Json.toJson(renewal)


  val emptyRequestWithHeader = FakeRequest().withHeaders("Accept" -> "application/vnd.hmrc.1.0+json")

  def fakeRequest(body:JsValue) = FakeRequest(POST, "url").withBody(body)
    .withHeaders("Content-Type" -> "application/json")

  lazy val renewalRequest = fakeRequest(renewalJsonBody).withHeaders("Accept" -> "application/vnd.hmrc.1.0+json")
  lazy val renewalRequestNoAcceptHeader = fakeRequest(renewalJsonBody)
  lazy val renewalBadRequest = fakeRequest(Json.toJson("Something Incorrect")).withHeaders("Accept" -> "application/vnd.hmrc.1.0+json")

  val authConnector = new TestAuthConnector(Some(nino))
  val taiConnector = new TestTaiConnector(taxSummaryDetails)
  val ntcConnector = new TestNtcConnector(Success(200))
  val testAccess = new TestAccessCheck(authConnector)
  val testCompositeAction = new TestAccountAccessControlWithAccept(testAccess)
  val testPersonalIncomeService = new TestPersonalIncomeService(taiConnector, authConnector, ntcConnector)

  val testSandboxPersonalIncomeService = SandboxPersonalIncomeService
  val sandboxCompositeAction = AccountAccessControlForSandbox
}

trait Success extends Setup {
  val controller = new PersonalIncomeController {
    override val service: PersonalIncomeService = testPersonalIncomeService
    override val accessControl: AccountAccessControlWithHeaderCheck = testCompositeAction
  }
}

trait AuthWithoutNino extends Setup {

  override val authConnector =  new TestAuthConnector(None) {
    override def hasNino()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = Future.failed(new uk.gov.hmrc.play.http.Upstream4xxResponse("Error", 401, 401))
  }

  override val testAccess = new TestAccessCheck(authConnector)
  override val taiConnector = new TestTaiConnector(taxSummaryDetails)
  override val testCompositeAction = new TestAccountAccessControlWithAccept(testAccess)
  override val testPersonalIncomeService = new TestPersonalIncomeService(taiConnector, authConnector, ntcConnector)

  val controller = new PersonalIncomeController {
    override val service: PersonalIncomeService = testPersonalIncomeService
    override val accessControl: AccountAccessControlWithHeaderCheck = testCompositeAction
  }
}

trait SandboxSuccess extends Setup {
  val controller = new PersonalIncomeController {
    override val service: PersonalIncomeService = testSandboxPersonalIncomeService
    override val accessControl: AccountAccessControlWithHeaderCheck = sandboxCompositeAction
  }
}
