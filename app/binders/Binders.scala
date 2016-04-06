package binders

import play.api.mvc.PathBindable
import uk.gov.hmrc.domain.Nino

object Binder {

  implicit def ninoBinder(implicit stringBinder: PathBindable[String]) = new PathBindable[Nino] {

    def unbind(key: String, nino: Nino): String = stringBinder.unbind(key, nino.value)

    def bind(key: String, value: String): Either[String, Nino] = {
      Nino.isValid(value) match {
        case true => Right(Nino(value))
        case false => Left("ERROR_NINO_INVALID")
      }
    }
  }
}