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

package uk.gov.hmrc.personalincome.controllers

import uk.gov.hmrc.api.controllers.ErrorResponse
import uk.gov.hmrc.http.HttpException


case object ErrorNinoInvalid extends ErrorResponse(400, "NINO_INVALID", "The provided NINO is invalid")
case object ErrorUnauthorizedNoNino extends ErrorResponse(401, "UNAUTHORIZED", "NINO does not exist on account")
case object ErrorwithNtcRenewal extends ErrorResponse(500, "NTC_RENEWAL_ERROR", "Failed to process renewal")
case object ErrorwithNtcRenewalAuthentication extends ErrorResponse(500, "NTC_RENEWAL_AUTH_ERROR", "Failed to obtain renewal auth token")
case object ErrorNoAuthToken extends ErrorResponse(500, "NTC_RENEWAL_AUTH_ERROR", "No auth header supplied in http request")
case object ErrorAuthTokenSupplied extends ErrorResponse(500, "NTC_RENEWAL_AUTH_ERROR", "Auth header is not required in the request")
case object ClientRetryRequest extends ErrorResponse(429, "NTC_RETRY", "Client must retry the request.")

class GrantAccessException(message: String) extends HttpException(message, 401)

class FailToMatchTaxIdOnAuth extends GrantAccessException("Unauthorised! Failure to match URL NINO against Auth NINO")

class NinoNotFoundOnAccount extends GrantAccessException("Unauthorised! NINO not found on account!")

class AccountWithLowCL extends GrantAccessException("Unauthorised! Account with low CL!")

class AccountWithWeakCredStrength(message:String) extends uk.gov.hmrc.http.HttpException(message, 401)

