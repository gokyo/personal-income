package connectors

import config.{AppContext, WSHttp}
import domain.Example
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.HttpGet

import scala.concurrent.Future

trait ExampleBackendConnector {

  val http: HttpGet
  val desUrl: String

  def fetchExample(saUtr: SaUtr)(implicit hc: HeaderCarrier): Future[Example] =
    http.GET[Example](s"${desUrl}/des-example-service/sa/${saUtr.utr}/example")
}

object ExampleBackendConnector extends ExampleBackendConnector {
  override val http: HttpGet = WSHttp
  override val desUrl: String = AppContext.desUrl
}