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

package uk.gov.hmrc.personalincome.stubs

import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.domain.Nino

object AuthStub {

  private val oid: String = "123456789abcdef012345678"

  def authRecordExists(nino: Nino, confidenceLevel: Int = 200, credentialStrength: String = "strong"): Unit = {
    stubFor(get(urlEqualTo("/auth/authority"))
      .willReturn(aResponse()
        .withStatus(200)
        .withBody(
          s"""
             |{
             |  "uri": "/auth/oid/$oid",
             |  "confidenceLevel": $confidenceLevel,
             |  "credentialStrength": "$credentialStrength",
             |  "nino": "${nino.value}",
             |  "userDetailsLink": "http://localhost:9978/user-details/id/59db4a285800005800576244",
             |  "legacyOid": "$oid",
             |  "new-session": "/auth/oid/$oid/session",
             |  "ids": "/auth/oid/$oid/ids",
             |  "credentials": {
             |    "gatewayId": "ghfkjlgkhl"
             |  },
             |  "accounts": {
             |    "fandf": {
             |      "nino": "${nino.value}",
             |      "link": "/fandf/${nino.value}"
             |    },
             |    "tai": {
             |      "nino": "${nino.value}",
             |      "link": "/tai/${nino.value}"
             |    },
             |    "nisp": {
             |      "nino": "${nino.value}",
             |      "link": "/nisp/${nino.value}"
             |    },
             |    "paye": {
             |      "nino": "${nino.value}",
             |      "link": "/paye/${nino.value}"
             |    },
             |    "tcs": {
             |      "nino": "${nino.value}",
             |      "link": "/tcs/${nino.value}"
             |    },
             |    "iht": {
             |      "nino": "${nino.value}",
             |      "link": "/iht/${nino.value}"
             |    }
             |  },
             |  "lastUpdated": "2017-10-09T10:06:34.190Z",
             |  "loggedInAt": "2017-10-09T10:06:34.190Z",
             |  "levelOfAssurance": "1.5",
             |  "enrolments": "/auth/oid/$oid/enrolments",
             |  "affinityGroup": "Individual",
             |  "correlationId": "8be9ca431b4b8ef3f584990d130270a84c1dbfe2d3e6c23f212d1a52f4c1f926",
             |  "credId": "cred-id"
             |}
           """.stripMargin)))
  }

}
