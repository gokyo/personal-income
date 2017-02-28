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

package uk.gov.hmrc.personalincome.connectors

import play.api.Logger
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.personalincome.config.{ServicesCircuitBreaker, WSHttp}
import uk.gov.hmrc.personaltaxsummary.domain.PersonalTaxSummaryContainer
import uk.gov.hmrc.personaltaxsummary.viewmodels.{PTSEstimatedIncomeViewModel, PTSYourTaxableIncomeViewModel}
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}

trait PersonalTaxSummaryConnector {
  this: ServicesCircuitBreaker =>

  val externalServiceName = "personal-tax-summary"

  def http: HttpGet with HttpPost

  def serviceUrl: String

  def url(path: String) = s"$serviceUrl$path"

  def buildEstimatedIncome(nino: Nino, container: PersonalTaxSummaryContainer, journeyId: Option[String] = None)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext) : Future[PTSEstimatedIncomeViewModel] = {
    Logger.debug(s"PersonalTaxSummary - POST to /personal-tax/$nino/buildestimatedincome ")
    withCircuitBreaker(
      http.POST[PersonalTaxSummaryContainer, PTSEstimatedIncomeViewModel](url = url(s"/personal-tax/$nino/buildestimatedincome"), body = container)
    )
  }

  def buildYourTaxableIncome(nino: Nino, container: PersonalTaxSummaryContainer, journeyId: Option[String] = None)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext) : Future[PTSYourTaxableIncomeViewModel]= {
    Logger.debug(s"PersonalTaxSummary - POST to /personal-tax/$nino/buildyourtaxableincome ")
    withCircuitBreaker(
      http.POST[PersonalTaxSummaryContainer, PTSYourTaxableIncomeViewModel](url = url(s"/personal-tax/$nino/buildyourtaxableincome"), body = container)
    )
  }
}

object PersonalTaxSummaryConnector extends PersonalTaxSummaryConnector with ServicesConfig with ServicesCircuitBreaker {
  lazy val serviceUrl = baseUrl("personal-tax-summary")
  override def http = WSHttp
}
