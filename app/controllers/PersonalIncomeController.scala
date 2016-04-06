package controllers

import play.api.libs.json.Json
import services.{LivePersonalIncomeService, PersonalIncomeService, SandboxExampleService}
import uk.gov.hmrc.domain.Nino
import play.api.Logger
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.NotFoundException
import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global

trait PersonalIncomeController extends BaseController with HeaderValidator {
  val service: PersonalIncomeService
  implicit val hc: HeaderCarrier

  final def getSummary(nino:Nino,year:Int) = validateAccept(acceptHeaderValidationRules).async {
    service.getSummary(nino,year).map(as => Ok(Json.toJson(as))
    ) recover {
      case ex: NotFoundException => Status(ErrorNotFound.httpStatusCode)(Json.toJson(ErrorNotFound))
      case e: Throwable =>
        Logger.error(s"Internal server error: ${e.getMessage}", e)
        Status(ErrorInternalServerError.httpStatusCode)(Json.toJson(ErrorInternalServerError))
    }
  }
}

object SandboxPersonalIncomeController extends PersonalIncomeController {
  override val service = SandboxExampleService
  override implicit val hc: HeaderCarrier = HeaderCarrier()
}

object LivePersonalIncomeController extends PersonalIncomeController {
  override val service = LivePersonalIncomeService
  override implicit val hc: HeaderCarrier = HeaderCarrier()
}