package connectors

import config.{AppContext, WSHttp}
import domain.Registration
import play.api.Logger
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpPost}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait ServiceLocatorConnector {
  val appName: String
  val appUrl: String
  val serviceUrl: String
  val handlerOK: () => Unit
  val handlerError: Throwable => Unit

  val metadata: Option[Map[String, String]]


  val http: HttpPost


  def register(implicit hc: HeaderCarrier): Future[Boolean] = {
    val registration = Registration(appName, appUrl, metadata)
    http.POST(s"$serviceUrl/registration", registration, Seq("Content-Type" -> "application/json")) map {
      _ =>
        handlerOK()
        true
    } recover {
      case e: Throwable =>
        handlerError(e)
        false
    }
  }
}


object ServiceLocatorConnector extends ServiceLocatorConnector {
  override lazy val appName = AppContext.appName
  override lazy val appUrl = AppContext.appUrl
  override lazy val serviceUrl = AppContext.serviceLocatorUrl
  override val http: HttpPost = WSHttp
  override val handlerOK: () => Unit = () => Logger.info("Service is registered on the service locator")
  override val handlerError: Throwable => Unit = e => Logger.error(s"Service could not register on the service locator", e)
  override val metadata: Option[Map[String, String]] = Some(Map("third-party-api" -> "true"))
}


