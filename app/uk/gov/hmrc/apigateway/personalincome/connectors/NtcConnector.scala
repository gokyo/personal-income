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

package uk.gov.hmrc.apigateway.personalincome.connectors

import models.TcrRenewal
import uk.gov.hmrc.apigateway.personalincome.config.WSHttp
import uk.gov.hmrc.apigateway.personalincome.domain.TaxCreditsNino
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.http.ws.WSPost
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


sealed trait Response {
  def status:Int
}
case class Success(status:Int) extends Response
case class Error(status:Int) extends Response

trait NtcConnector {
  def http: WSPost = ???

  def serviceUrl: String

  def submitRenewal(nino: TaxCreditsNino,
                    renewalData: TcrRenewal)(implicit headerCarrier: HeaderCarrier): Future[Response] = {
    val uri = s"$serviceUrl/tcs/${nino.taxCreditsNino}/renewal"
    http.doPost[TcrRenewal](uri, renewalData, Seq()).map(response => {
      response.status match {
        case x if x >= 200 && x < 300 => Success(x)
        case _ => Error(response.status)
      }
    })
  }
}

object NtcConnector extends NtcConnector with ServicesConfig {
  override val http = WSHttp

  override lazy val serviceUrl = baseUrl("ntc")
}
