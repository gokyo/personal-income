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

package uk.gov.hmrc.personalincome.support

import org.scalatest.{Matchers, OptionValues, WordSpec}
import org.scalatestplus.play.WsScalaTestClient
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.ws.WSClient
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.controllers.HeaderKeys.tcrAuthToken
import uk.gov.hmrc.personalincome.domain.RenewalReference
import uk.gov.hmrc.personalincome.domain.TcrAuthenticationToken.basicAuthString

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class BaseISpec extends WordSpec with Matchers with OptionValues with WsScalaTestClient with GuiceOneServerPerSuite with WireMockSupport {
  override implicit lazy val app: Application = appBuilder.build()

  protected val nino1 = Nino("AA000000A")
  protected val nino2 = Nino("AP412713B")
  protected val acceptJsonHeader: (String, String) = "Accept" -> "application/vnd.hmrc.1.0+json"
  protected val tcrAuthTokenHeader: (String, String) = tcrAuthToken -> tcrAuthenticationToken
  protected val renewalReference: RenewalReference = RenewalReference("renewalReference")
  protected val tcrAuthenticationToken: String = basicAuthString(nino1.value, renewalReference.value)

  def config: Map[String, Any] = Map(
    "auditing.enabled" -> false,
    "microservice.services.service-locator.enabled" -> false,
    "microservice.services.auth.port" -> wireMockPort,
    "microservice.services.datastream.port" -> wireMockPort,
    "microservice.services.ntc.port" -> wireMockPort,
    "microservice.services.personal-tax-summary.port" -> wireMockPort,
    "microservice.services.service-locator.port" -> wireMockPort,
    "microservice.services.tai.port" -> wireMockPort,
    "microservice.services.tax-credits-broker.port" -> wireMockPort)

  protected def appBuilder: GuiceApplicationBuilder = new GuiceApplicationBuilder().configure(config)

  protected implicit lazy val wsClient: WSClient = app.injector.instanceOf[WSClient]

  implicit val defaultTimeout: FiniteDuration = 5 seconds

  def await[A](future: Future[A])(implicit timeout: Duration): A = Await.result(future, timeout)
}
