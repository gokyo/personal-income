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

package uk.gov.hmrc.personalincome.controllers.action

import uk.gov.hmrc.auth.core.retrieve.Retrievals.{nino, _}
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.auth.core.{AuthorisedFunctions, Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.personalincome.controllers.{AccountWithLowCL, FailToMatchTaxIdOnAuth, NinoNotFoundOnAccount}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class Authority(nino: Nino)

trait Authorisation extends AuthorisedFunctions {

  val confLevel: Int

  lazy val ninoNotFoundOnAccount = new NinoNotFoundOnAccount
  lazy val failedToMatchNino = new FailToMatchTaxIdOnAuth
  lazy val lowConfidenceLevel = new AccountWithLowCL

  def grantAccess(requestedNino: Nino)(implicit hc: HeaderCarrier): Future[Authority] = {
    authorised(Enrolment("HMRC-NI", Seq(EnrolmentIdentifier("NINO", requestedNino.value)), "Activated", None))
      .retrieve(nino and confidenceLevel) {
        case Some(nino) ~ confidenceLevel ⇒ {
          if (nino.isEmpty) throw ninoNotFoundOnAccount
          if (!nino.equals(requestedNino.nino)) throw failedToMatchNino
          if (confLevel > confidenceLevel.level) throw lowConfidenceLevel
          Future(Authority(requestedNino))
        }
        case None ~ _ ⇒ {
          throw ninoNotFoundOnAccount
        }
      }
  }
}