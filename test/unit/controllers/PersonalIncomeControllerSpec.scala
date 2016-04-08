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

import uk.gov.hmrc.apigateway.personalincome.connectors.{AuthConnector, TaiConnector}
import uk.gov.hmrc.apigateway.personalincome.controllers.PersonalIncomeController
import uk.gov.hmrc.apigateway.personalincome.controllers.action.{AccountAccessControlForSandbox, AccountAccessControlWithHeaderCheck, AccountAccessControl}
import uk.gov.hmrc.apigateway.personalincome.domain.{Accounts, TaxSummaryDetails}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeApplication, FakeRequest}
import uk.gov.hmrc.apigateway.personalincome.services.SandboxPersonalIncomeService._
import uk.gov.hmrc.apigateway.personalincome.services.{SandboxPersonalIncomeService, PersonalIncomeService, LivePersonalIncomeService}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.auth.microservice.connectors.ConfidenceLevel
import uk.gov.hmrc.play.http.{UnauthorizedException, HeaderCarrier, HttpGet, HttpPost}
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.{ExecutionContext, Future}


class TestPersonalIncomeSpec extends UnitSpec with WithFakeApplication with ScalaFutures with BeforeAndAfterEach with StubApplicationConfiguration {


  class TestTaiConnector(taxSummaryDetails:TaxSummaryDetails) extends TaiConnector {
    override def http: HttpGet with HttpPost = ???

    override def serviceUrl: String = ???

    override def taxSummary(nino : Nino, year : Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
      Future.successful(taxSummaryDetails)
    }
  }

  class TestAuthConnector(nino:Option[Nino]) extends AuthConnector {
    override val serviceUrl: String = "someUrl"

    override def serviceConfidenceLevel: ConfidenceLevel = ???

    override def http: HttpGet = ???

    override def accounts()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Accounts] = Future(Accounts(nino, None))
    override def hasNino()(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] = Future(Unit)
  }

  class TestPersonalIncomeService(testTaiConnector:TestTaiConnector, testAuthConnector:TestAuthConnector) extends LivePersonalIncomeService {
    var saveNino:Option[String]=None
    var saveYear:Option[Int]=None

    override val taiConnector = testTaiConnector
    override val authConnector= testAuthConnector

    override def audit(method:String, nino:Nino, year:Int, details:Map[String, String])(implicit hc: HeaderCarrier): Unit = {
      saveNino=Some(nino.value)
      saveYear=Some(year)
    }
  }

  class TestAccessCheck(testAuthConnector:TestAuthConnector) extends AccountAccessControl {
    override val authConnector: AuthConnector = testAuthConnector
  }

  class TestAccountAccessControlWithAccept(testAccessCheck:AccountAccessControl) extends AccountAccessControlWithHeaderCheck {
    override val accessControl: AccountAccessControl = testAccessCheck
  }


  trait Setup {
    implicit val hc = HeaderCarrier()

    val emptyRequest = FakeRequest()
    val emptyRequestWithHeader = FakeRequest().withHeaders("Accept" -> "application/vnd.hmrc.1.0+json")

    val nino = Nino("CS700100A")
    val taxSummaryDetails = TaxSummaryDetails(nino.value,1)

    val authConnector = new TestAuthConnector(Some(nino))
    val taiConnector = new TestTaiConnector(taxSummaryDetails)
    val testAccess = new TestAccessCheck(authConnector)
    val testCompositeAction = new TestAccountAccessControlWithAccept(testAccess)
    val testPersonalIncomeService = new TestPersonalIncomeService(taiConnector, authConnector)

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
    override val testPersonalIncomeService = new TestPersonalIncomeService(taiConnector, authConnector)

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

  override lazy val fakeApplication = FakeApplication(additionalConfiguration=config)

  "getSummary Sandbox" should {

    "return the summary response from a resource" in new SandboxSuccess {
      val year = 2016
      val result = await(controller.getSummary(nino,2016)(emptyRequestWithHeader))

      status(result) shouldBe 200

      val resource = findResource(s"/resources/getsummary/${nino.value}_$year.json")
      contentAsJson(result) shouldBe Json.parse(resource.get)

      testPersonalIncomeService.saveNino shouldBe None
      testPersonalIncomeService.saveYear shouldBe None
    }

    "return the static resource since the supplied resource cannot be resolved" in new SandboxSuccess {
      val year = 2016
      val result = await(controller.getSummary(nino,2018)(emptyRequestWithHeader))

      status(result) shouldBe 200

      contentAsJson(result) shouldBe Json.toJson(TaxSummaryDetails(nino.value, 2018))

      testPersonalIncomeService.saveNino shouldBe None
      testPersonalIncomeService.saveYear shouldBe None
    }

  }

  "getSummary Live" should {

    "return the summary successfully" in new Success {

      val result = await(controller.getSummary(nino,90)(emptyRequestWithHeader))

      status(result) shouldBe 200
      contentAsJson(result) shouldBe Json.toJson(taxSummaryDetails)

      testPersonalIncomeService.saveNino.get shouldBe nino.value
      testPersonalIncomeService.saveYear.get shouldBe 90
    }

    "Return unauthorized when authority record does not contain a NINO" in new AuthWithoutNino {
      val result = await(controller.getSummary(nino,90)(emptyRequestWithHeader))

      status(result) shouldBe 401
    }

    "return status code 406 when the headers are invalid" in new Success {
      val result = await(controller.getSummary(nino,90)(emptyRequest))

      status(result) shouldBe 406
    }
  }

}
