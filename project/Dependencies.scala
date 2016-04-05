import play.PlayImport._
import play.core.PlayVersion
import sbt._

object Dependencies {

  val testScope: String = "test,it"

  val playWs = ws exclude("org.apache.httpcomponents", "httpclient") exclude("org.apache.httpcomponents", "httpcore")
  val microserviceBootStrap = "uk.gov.hmrc" %% "microservice-bootstrap" % "4.2.1"
  val playAuthorisation = "uk.gov.hmrc" %% "play-authorisation" % "3.1.0"
  val playHealth = "uk.gov.hmrc" %% "play-health" % "1.1.0"
  val playUrlBinders = "uk.gov.hmrc" %% "play-url-binders" % "1.0.0"
  val playConfig = "uk.gov.hmrc" %% "play-config" % "2.0.1"
  val playJsonLogger = "uk.gov.hmrc" %% "play-json-logger" % "2.1.1"
  val domain = "uk.gov.hmrc" %% "domain" % "3.5.0"
  val referenceChecker = "uk.gov.hmrc" %% "reference-checker" % "2.0.0"
  val hmrcTest = "uk.gov.hmrc" %% "hmrctest" % "1.6.0" % testScope
  val scalaTest = "org.scalatest" %% "scalatest" % "2.2.2" % testScope
  val pegDown = "org.pegdown" % "pegdown" % "1.4.2" % testScope
  val playTest = "com.typesafe.play" %% "play-test" % PlayVersion.current % testScope
  val scalaTestPlus = "org.scalatestplus" %% "play" % "1.2.0" % testScope
  val cucumberScala = "info.cukes" %% "cucumber-scala" % "1.2.4" % testScope
  val cucumberJUnit = "info.cukes" % "cucumber-junit" % "1.2.4" % testScope
  val scalaHttp = "org.scalaj" %% "scalaj-http" % "1.1.5"
  val junit = "junit" % "junit" % "4.12" % testScope
  val wireMock = "com.github.tomakehurst" % "wiremock" % "1.48" % testScope exclude("org.apache.httpcomponents", "httpclient") exclude("org.apache.httpcomponents", "httpcore")

  val compileDependencies = Seq(microserviceBootStrap, playAuthorisation, playHealth, playUrlBinders, playConfig, playJsonLogger, domain, referenceChecker)
  val testDependencies = Seq(hmrcTest, scalaTest, pegDown, playTest, cucumberScala, cucumberJUnit, scalaHttp, junit, wireMock)

}