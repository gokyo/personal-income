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

package uk.gov.hmrc.personalincome.connectors

import play.Logger
import uk.gov.hmrc.personalincome.config.{ServicesCircuitBreaker, WSHttp}
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.play.http._

import scala.concurrent.{ExecutionContext, Future}

sealed trait Response {
  def status:Int
}
case class Success(status:Int) extends Response
case class Error(status:Int) extends Response

trait NtcConnector {
  this: ServicesCircuitBreaker =>

  val externalServiceName = "ntc"

  def http: HttpGet with HttpPost

  def serviceUrl: String

  def authenticateRenewal(nino: TaxCreditsNino,
                          renewalReference: RenewalReference)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Option[TcrAuthenticationToken]] = {

    def logResult(status:Int, message:String): Unit = {
      Logger.info(s"Response from tcs auth service $status and message $message.")
    }

    withCircuitBreaker(
      http.GET[Option[TcrAuthenticationToken]](s"$serviceUrl/tcs/${nino.value}/${renewalReference.value}/auth").recover {
        case ex : NotFoundException =>
          logResult(404, "Not found")
          None

        case ex : BadRequestException =>
          logResult(400, "BadRequest")
          None
      })
  }

  def claimantDetails(nino: TaxCreditsNino)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[ClaimantDetails] = {
    withCircuitBreaker(http.GET[ClaimantDetails](s"$serviceUrl/tcs/${nino.value}/claimant-details"))
  }

  def submitRenewal(nino: TaxCreditsNino,
                    renewalData: TcrRenewal)(implicit headerCarrier: HeaderCarrier, ex: ExecutionContext): Future[Response] = {
    val uri = s"$serviceUrl/tcs/${nino.taxCreditsNino}/renewal"
    withCircuitBreaker(http.POST[TcrRenewal, HttpResponse](uri, renewalData, Seq()).map(response => {
      response.status match {
        case x if x >= 200 && x < 300 => Success(x)
        case _ => Error(response.status)
      }
    }))
  }

}

object NtcConnector extends NtcConnector with ServicesConfig with ServicesCircuitBreaker {
  override val http = WSHttp

  override lazy val serviceUrl = baseUrl("ntc")
}