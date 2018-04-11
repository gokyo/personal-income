/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.personalincome.stubs

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.personalincome.connectors.{NtcConnector, Response}
import uk.gov.hmrc.personalincome.domain._
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

trait NtcConnectorStub extends UnitSpec {

  def stubAuthenticateRenewal(response: Option[TcrAuthenticationToken])(implicit ntcConnector: NtcConnector) = {
    when(ntcConnector.authenticateRenewal(any[TaxCreditsNino](), any[RenewalReference]())(any[HeaderCarrier](), any[ExecutionContext]()))
      .thenReturn(Future(response))
  }

  def stubClaimantClaims(response: Claims)(implicit ntcConnector: NtcConnector) = {
    when(ntcConnector.claimantClaims(any[TaxCreditsNino]())(any[HeaderCarrier](), any[ExecutionContext]()))
      .thenReturn(Future(response))
  }

  def stubSubmitRenewals(response: Response)(implicit  ntcConnector: NtcConnector) = {
    when(ntcConnector.submitRenewal(any[TaxCreditsNino](), any[TcrRenewal]())(any[HeaderCarrier](), any[ExecutionContext]()))
      .thenReturn(Future(response))
  }

  def stubSubmitRenewalsException(e: Exception)(implicit  ntcConnector: NtcConnector) = {
    when(ntcConnector.submitRenewal(any[TaxCreditsNino](), any[TcrRenewal]())(any[HeaderCarrier](), any[ExecutionContext]())).thenReturn(Future failed(e))
  }
}
