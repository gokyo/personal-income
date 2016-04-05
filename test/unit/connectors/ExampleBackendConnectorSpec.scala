package unit.connectors

import connectors.ExampleBackendConnector
import domain.Example
import org.mockito.Matchers
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mock.MockitoSugar
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.HttpGet
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.Future

class ExampleBackendConnectorSpec extends UnitSpec with MockitoSugar with ScalaFutures {

  trait Setup {
    implicit val hc = HeaderCarrier()

    val connector = new ExampleBackendConnector {
      override val http = mock[HttpGet]
      override val desUrl = "https://DES_HOST"
    }
  }

  "fetch Example with utr '2234567890K'" should {
    "return an Example Response" in new Setup {

      val expectedResponse = Example("example",1.0)

      when(connector.http.GET[Example](Matchers.eq("https://DES_HOST/des-example-service/sa/2234567890K/example"))(any(), any())).
        thenReturn(Future.successful(expectedResponse))
      connector.fetchExample(SaUtr("2234567890K")).futureValue shouldBe expectedResponse
      verify(connector.http).GET[Example](Matchers.eq("https://DES_HOST/des-example-service/sa/2234567890K/example"))(any(), any())
    }
  }
}
