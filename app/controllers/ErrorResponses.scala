package controllers

sealed abstract class ErrorResponse(
                                     val httpStatusCode: Int,
                                     val errorCode: String,
                                     val message: String)

case object ErrorNinoInvalid extends ErrorResponse(400, "NINO_INVALID", "The provided NINO is invalid")

case object ErrorUnauthorized extends ErrorResponse(401, "UNAUTHORIZED", "Bearer token is missing or not authorized")

case object ErrorNotFound extends ErrorResponse(404, "NOT_FOUND", "Resource was not found")

case object ErrorGenericBadRequest extends ErrorResponse(400, "BAD_REQUEST", "Bad Request")

case object ErrorAcceptHeaderInvalid extends ErrorResponse(406, "ACCEPT_HEADER_INVALID", "The accept header is missing or invalid")

case object ErrorInternalServerError extends ErrorResponse(500, "INTERNAL_SERVER_ERROR", "Internal server error")



