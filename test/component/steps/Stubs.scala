package component.steps

import com.github.tomakehurst.wiremock.client.WireMock._

object DES  {

  def found(requestUrl: String, response: String) = {
    stubFor(get(urlEqualTo(s"$requestUrl"))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody(response)))
  }

  def error(requestUrl: String,status: Int) = {
    stubFor(get(urlEqualTo(s"$requestUrl"))
      .willReturn(
        aResponse().
          withStatus(status).
          withBody("Some error in DES")))
  }

}

object Auditing {

  def create() = stubFor(post(urlEqualTo("/write/audit"))
      .willReturn(
        aResponse()
          .withStatus(200)))

  def error() = stubFor(post(urlEqualTo("/write/audit"))
    .willReturn(
      aResponse()
        .withStatus(500)))

}



object Auth  {

  private def authBody(utr: String) =
    s"""
       |{
       |  "uri": "/auth/oid/2234567890",
       |  "loggedInAt" : "2014-06-09T14:57:09.522Z",
       |  "previouslyLoggedInAt" : "2014-06-09T14:57:09.522Z",
       |  "accounts": {
       |   "sa" : {"link": "/sa/individual/$utr","utr": "$utr}"
                                                               |  }
                                                               |}
     """.stripMargin

  def authoriseForSA(utr:String) = {

    stubFor(get(urlEqualTo(s"/authorise/read/sa/$utr?confidenceLevel=50"))
      .withHeader("Authorization", equalTo("Bearer LongBearerScrambledLiteral"))
      .willReturn(
        aResponse()
          .withStatus(200)
          .withBody(authBody(utr))))

  }

  def unAuthoriseForSA(utr:String) = {

    stubFor(get(urlEqualTo(s"/authorise/read/sa/$utr?confidenceLevel=50"))
      .withHeader("Authorization", equalTo("Bearer WrongBearerToken"))
      .willReturn(
        aResponse()
          .withStatus(401)
      .withBody("""{"test":"test"}""")))

  }


}
