package services

import config.MicroserviceAuditConnector
import connectors.ExampleBackendConnector
import domain._
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ExampleService {

  def fetchExample(saUtr: SaUtr)(implicit hc: HeaderCarrier): Future[Example]

}

trait LiveExampleService extends ExampleService {
  val connector: ExampleBackendConnector

  override def fetchExample(saUtr: SaUtr)(implicit hc: HeaderCarrier): Future[Example] = {

    def auditResponse(): Unit = {
      MicroserviceAuditConnector.sendEvent(
        DataEvent("api-microservice-template", "ServiceResponseSent",
          tags = Map("transactionName" -> "fetchExample"),
          detail = Map("saUtr" -> saUtr.utr)))
    }

    connector.fetchExample(saUtr).map {
      ex => auditResponse()
        ex
    }
  }

}


object SandboxExampleService extends ExampleService {
  override def fetchExample(saUtr: SaUtr)(implicit hc: HeaderCarrier): Future[Example] = {
    Future.successful(Example("example", 1.0))
  }
}

object LiveExampleService extends LiveExampleService {
  override val connector = ExampleBackendConnector
}
