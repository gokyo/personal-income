package unit.config

import config.ServiceLocatorRegistration
import connectors.ServiceLocatorConnector
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.test.FakeApplication
import play.api.{Application, GlobalSettings}
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class RegisterInServiceLocatorSpec extends UnitSpec with MockitoSugar {




  trait Setup extends ServiceLocatorRegistration {
    val mockConnector = mock[ServiceLocatorConnector]
    override val slConnector = mockConnector
    override implicit val hc: HeaderCarrier = HeaderCarrier()
    val fakeApplicationWithGlobal = FakeApplication(withGlobal = Some(new GlobalSettings() {
      override def onStart(app: Application) { super.onStart(app) }
    }))
  }

  "onStart" should {
    "register the microservice in service locator when registration is enabled" in new Setup {
      override val registrationEnabled: Boolean = true

      when(mockConnector.register(any())).thenReturn(Future.successful(true))
      onStart(fakeApplicationWithGlobal)
      verify(mockConnector).register(any())
    }


    "not register the microservice in service locator when registration is disabled" in new Setup {
      override val registrationEnabled: Boolean = false
      onStart(fakeApplicationWithGlobal)
      verify(mockConnector,never()).register(any())
    }
  }
}
