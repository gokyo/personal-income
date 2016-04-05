package component.steps

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.core.WireMockConfiguration._
import cucumber.api.scala.{EN, ScalaDsl}
import org.scalatest.Matchers
import play.api.Mode
import play.api.test.{FakeApplication, TestServer}

trait Env extends ScalaDsl with EN with Matchers {

  val port = 9000
  var host = s"http://localhost:$port"

  val stubPort = sys.env.getOrElse("WIREMOCK_PORT", "11111").toInt
  val stubHost = "localhost"

  val wireMockUrl = s"http://$stubHost:$stubPort"
  final val wireMockServer = new WireMockServer(wireMockConfig().port(stubPort))
  var server: TestServer = null

  Runtime.getRuntime addShutdownHook new Thread { override def run { shutdown() } }

  def shutdown() = {
    wireMockServer.stop()
    server.stop()
  }

  Before { scenario =>

    if (!wireMockServer.isRunning) {
      wireMockServer.start()
    }

    WireMock.configureFor(stubHost, stubPort)
  }

  After { scenario =>
    if (wireMockServer.isRunning) WireMock.reset()
  }

  def startServer() = {
    server = new TestServer(port, new FakeApplication(additionalConfiguration = Map("run.mode" -> "Stub")) {
      override val mode = Mode.Prod
    })
    server.start()
  }
}

object Env extends Env