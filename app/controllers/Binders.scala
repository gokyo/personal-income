package controllers

import play.api.mvc.PathBindable
import uk.gov.hmrc.domain.Nino

object Binders {

  implicit def ninoBinder(implicit stringBinder: PathBindable[String]) = new PathBindable[Nino] {

    def unbind(key: String, saUtr: Nino): String = stringBinder.unbind(key, saUtr.value)

    def bind(key: String, value: String): Either[String, Nino] = {
      Nino.isValid(value) match {
        case true => Right(Nino(value))
        case false => Left("ERROR_NINO_INVALID")
      }
    }
  }
}
