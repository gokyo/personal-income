package controllers

import play.api.libs.json.Json
import services.{ExampleService, LiveExampleService, SandboxExampleService}
import uk.gov.hmrc.domain.SaUtr
import play.api.Logger
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.NotFoundException
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global

trait ExampleController extends BaseController with HeaderValidator {
  val service: ExampleService
  implicit val hc: HeaderCarrier

  final def fetchExample(utr: SaUtr) = validateAccept(acceptHeaderValidationRules).async {
    service.fetchExample(utr).map(as => Ok(Json.toJson(as))
    ) recover {
      case ex: NotFoundException => Status(ErrorNotFound.httpStatusCode)(Json.toJson(ErrorNotFound))
      case e: Throwable =>
        Logger.error(s"Internal server error: ${e.getMessage}", e)
        Status(ErrorInternalServerError.httpStatusCode)(Json.toJson(ErrorInternalServerError))
    }
  }
}

object SandboxExampleController extends ExampleController {
  override val service = SandboxExampleService
  override implicit val hc: HeaderCarrier = HeaderCarrier()
}

object LiveExampleController extends ExampleController {
  override val service = LiveExampleService
  override implicit val hc: HeaderCarrier = HeaderCarrier()
}