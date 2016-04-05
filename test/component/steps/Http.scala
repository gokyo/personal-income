package component.steps

import scalaj.http.HttpRequest


object Request {

  sealed trait TokenStatus
  case object UndefinedToken extends TokenStatus
  case object HappyToken extends TokenStatus
  case object MissingToken extends TokenStatus
  case object WrongToken extends TokenStatus

  sealed trait AcceptHeader
  case object AcceptValid extends AcceptHeader
  case object AcceptMissing extends AcceptHeader
  case object AcceptBadFormat extends AcceptHeader
  case object AcceptUndefined extends AcceptHeader

  implicit class RequestBuilder(httpRequest: HttpRequest) {
    def addToken(tokenStatus: TokenStatus): HttpRequest = {
      tokenStatus match {
        case HappyToken => httpRequest.header("Authorization", "Bearer LongBearerScrambledLiteral")
        case WrongToken => httpRequest.header("Authorization", "Bearer WrongBearerToken")
        case MissingToken => httpRequest
        case UndefinedToken =>  throw new scala.RuntimeException("Undefined status in the scenario - no token status defined")
      }
    }

    def addAcceptHeader(acceptHeader: AcceptHeader): HttpRequest = {
      acceptHeader match {
        case AcceptMissing => httpRequest.header("Accept", "")
        case AcceptValid => httpRequest.header("Accept", "application/vnd.hmrc.1.0+json")
        case AcceptBadFormat => httpRequest.header("Accept", "application/vnd.hmrc.1.0+XML") // XML
        case AcceptUndefined => throw new scala.RuntimeException("Undefined accept in the scenario - no accept status defined")
      }
    }

  }

}


object Responses {
  val statusCodes = Map(
    "OK" -> 200,
    "UNSUPPORTED_MEDIA_TYPE" -> 415,
    "CONFLICT" -> 409,
    "REQUEST_TIMEOUT" -> 408,
    "NOT_FOUND" -> 404,
    "BAD_REQUEST" -> 400,
    "UNAUTHORIZED" -> 401,
    "NOT_ACCEPTABLE" -> 406,
    "INTERNAL_SERVER_ERROR" -> 500,
    "BAD_GATEWAY" -> 502
  )

}
