package config

import play.api.Play._
import uk.gov.hmrc.play.config.ServicesConfig

object AppContext extends ServicesConfig {
  lazy val appName = current.configuration.getString("appName").getOrElse(throw new RuntimeException("appName is not configured"))
  lazy val appUrl = current.configuration.getString("appUrl").getOrElse(throw new RuntimeException("appUrl is not configured"))
  lazy val serviceLocatorUrl: String = baseUrl("service-locator")
  lazy val desUrl: String = baseUrl("des")
  lazy val registrationEnabled: Boolean = current.configuration.getBoolean(s"${env}.microservice.services.service-locator.enabled").getOrElse(true)
}
