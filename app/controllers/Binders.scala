package controllers

import play.api.mvc.PathBindable
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.referencechecker.SelfAssessmentReferenceChecker

object Binders {

  implicit def saUtrBinder(implicit stringBinder: PathBindable[String]) = new PathBindable[SaUtr] {

    def unbind(key: String, saUtr: SaUtr): String = stringBinder.unbind(key, saUtr.value)

    def bind(key: String, value: String): Either[String, SaUtr] = {
      SelfAssessmentReferenceChecker.isValid(value) match {
        case true => Right(SaUtr(value))
        case false => Left("ERROR_SA_UTR_INVALID")
      }
    }
  }
}
