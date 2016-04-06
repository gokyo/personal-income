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

package component.steps

import component.steps.Request.{AcceptBadFormat, AcceptMissing, AcceptValid, MissingToken, WrongToken, _}
import cucumber.api.scala.{EN, ScalaDsl}
import org.scalatest.Matchers
import play.api.libs.json.Json

import scalaj.http.Http

object World {
  var responseCode: Int = 0
  var responseBody: String = ""
  var tokenStatus: TokenStatus = UndefinedToken
  var acceptHeader: AcceptHeader = AcceptUndefined
}


class ExampleServiceSteps extends ScalaDsl with EN with Matchers  {

  When( """^I am authorized with a valid Bearer token with utr '(.*)'$""") { (utr: String) =>
    Auth.authoriseForSA(utr)
    World.tokenStatus = HappyToken
  }

  When( """^I am not authorized due to '(.*)' Bearer token with utr '(.*)'$""") { (reason: String, utr: String) =>
    World.tokenStatus = reason match {
      case "missing" => MissingToken
      case "wrong" =>
        Auth.unAuthoriseForSA(utr)
        WrongToken
    }
  }

  Given( """^DES response to url '(.*)' is:$""") { (url:String, response: String) =>
    DES.found(url, response)
  }
  Given( """^DES response to url '(.*)' is error '(.*)'$""") { (url:String, error: String) =>
    DES.error(url, Responses.statusCodes(error))
  }


  When( """^I GET the LIVE resource '(.*)'$""") { (url: String) =>
    val response  = Http(s"${Env.host}$url").
      addAcceptHeader(World.acceptHeader).
      addToken(World.tokenStatus).asString

    World.responseCode = response.code
    World.responseBody = response.body
  }

  When( """^I GET the SANDBOX resource '(.*)'$""") { (url: String) =>
    val response = Http(s"${Env.host}$url").
      addAcceptHeader(World.acceptHeader).asString
    World.responseCode = response.code
    World.responseBody = response.body
  }

  Then( """^the status code should be '(.*)'$""") { (st: String) =>
    Responses.statusCodes(st) shouldBe World.responseCode
  }

  Given( """^header 'Accept' is '(.*)'$""") { (acceptValue: String) =>
    World.acceptHeader = acceptValue match {
      case "not provided" => AcceptMissing
      case "bad formatted" => AcceptBadFormat
      case "valid" => AcceptValid
      case _ => throw new scala.RuntimeException("Undefined value for accept in the step")
    }
  }

  Then( """^I should receive JSON response:$""") { (expectedResponseBody: String) =>
    Json.parse(expectedResponseBody) shouldBe Json.parse(World.responseBody)
  }

}
