package it.utils

import play.api.Play
import play.api.test.FakeApplication

trait MicroserviceLocalRunSugar {
  val additionalConfiguration: Map[String, Any]
  val localMicroserviceUrl = s"http://localhost:${port}"
  val port = sys.env.getOrElse("MICROSERVICE_PORT", "9001").toInt
  lazy val fakeApplication = FakeApplication(additionalConfiguration = additionalConfiguration)

  def run(block: () => Unit) = {
    Play.start(fakeApplication)
    block()
    Play.stop()
  }
}