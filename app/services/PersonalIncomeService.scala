package services

import config.MicroserviceAuditConnector
import connectors.TaiConnector
import domain._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.http.HeaderCarrier
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait PersonalIncomeService {
  def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails]
}

trait LivePersonalIncomeService extends PersonalIncomeService {
  val connector: TaiConnector

  def audit(method:String, nino:Nino, year:Int, details:Map[String, String]) = {
    def auditResponse(): Unit = {
      MicroserviceAuditConnector.sendEvent(
        DataEvent("personal-income", "ServiceResponseSent",
          tags = Map("transactionName" -> method),
          detail = details))
    }
  }

  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    connector.taxSummary(nino, year).map {
      resp => audit("getSummary", nino, year, Map("nino" -> nino.value, "year" -> year.toString))
        resp
    }
  }
}

object SandboxExampleService extends PersonalIncomeService {
  override def getSummary(nino: Nino, year:Int)(implicit hc: HeaderCarrier): Future[TaxSummaryDetails] = {
    Future.successful(TaxSummaryDetails("somevalue", 1))
  }
}

object LivePersonalIncomeService extends LivePersonalIncomeService {
  override val connector = TaiConnector
}
