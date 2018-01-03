/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.personalincome.domain

import com.ning.http.util.Base64
import play.api.libs.json.Json
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.http.BadRequestException

import scala.util.Try


case class LoginModel(nino: Nino, renewalReference: RenewalReference)

case class RenewalReference(value: String) {
  def stripSpaces = RenewalReference(value.replaceAll(" ", ""))
}

case class TcrAuthenticationToken(tcrAuthToken: String) {
  def extractNino : Option[String] = extractBasicAuth(tcrAuthToken).map(_._1)
  def extractRenewalReference : Option[String] = extractBasicAuth(tcrAuthToken).map(_._2)

  private def extractBasicAuth(auth: String): Option[(String, String)] = {
    val BasicAuthPattern = "Basic (.*)".r

    Try {
      auth match {
        case BasicAuthPattern(encoded) => {
          val parts = new String(Base64.decode(encoded)).split(":")
          (parts(0), parts(1))
        }
      }
    }.toOption
  }

}

object TcrAuthenticationToken {
  implicit val formats = Json.format[TcrAuthenticationToken]

  def basicAuthString(nino:String, renewalReference:String): String = "Basic " + encodedAuth(nino, renewalReference)

  def encodedAuth(nino:String, renewalReference:String): String = new String(Base64.encode(s"${nino}:${renewalReference}".getBytes))

}

object TcrAuthCheck {
  def unapply(token: String): Option[TcrAuthenticationToken] = {
    val tcrToken = TcrAuthenticationToken(token)
    if (tcrToken.extractNino.isDefined&&tcrToken.extractRenewalReference.isDefined) {
      Some(tcrToken)
    } else {
      None
    }
  }
}

case class ClaimantDetails(hasPartner: Boolean,
                           claimantNumber: Int,
                           renewalFormType: String,
                           mainApplicantNino: String,
                           usersPartnerNino: Option[String],
                           availableForCOCAutomation: Boolean,
                           applicationId: String)

object ClaimantDetails {
  implicit val formats = Json.format[ClaimantDetails]
}

case class SelfEmployedIncome(selfEmployedIncome: Option[Int], isSelfEmployedIncomeEst: Option[Boolean])
object SelfEmployedIncome {
  implicit val formats = Json.format[SelfEmployedIncome]
}

case class OtherIncome(otherHouseholdIncome: Option[Int], isOtherHouseholdIncomeEst: Option[Boolean])

object OtherIncome {
  implicit val formats = Json.format[OtherIncome]

  def fromStrings(otherHouseholdIncome:Option[Int], isOtherHouseholdIncomeEst:Boolean) = {
    val incomeEst= if (otherHouseholdIncome.isDefined) Some(isOtherHouseholdIncomeEst) else None
    OtherIncome(otherHouseholdIncome, incomeEst)
  }

  def toStrings(otherIncome:OtherIncome) = Some((otherIncome.otherHouseholdIncome,
    otherIncome.isOtherHouseholdIncomeEst.getOrElse(false)))
}

case class IncomeDetails(taxableBenefits: Option[Int],
                         earnings: Option[Int],
                         companyBenefits: Option[Int],
                         seProfits: Option[Int],
                         seProfitsEstimated:Option[Boolean]) {
  def total(): Int = {
    val income = Array(taxableBenefits, earnings, companyBenefits, seProfits)
    income.flatten.foldLeft(0)((b,a) => b+a)
  }
}

object IncomeDetails {

  implicit val formats = Json.format[IncomeDetails]

  def fromStrings(taxableBenefits:Option[Int], earnings:Option[Int], companyBenefits:Option[Int],seProfits:Option[Int],seProfitsEstimated:Boolean) :IncomeDetails = {
    val seProfitsEst=if(seProfits.isDefined) Some(seProfitsEstimated) else None
    IncomeDetails(taxableBenefits, earnings, companyBenefits, seProfits, seProfitsEst)
  }

  def toStrings(income:IncomeDetails) = {
    Some((income.taxableBenefits, income.earnings, income.companyBenefits, income.seProfits, income.seProfitsEstimated.getOrElse(false)))
  }

}

case class CertainBenefits(receivedBenefits: Boolean, incomeSupport: Boolean, jsa: Boolean, esa: Boolean, pensionCredit: Boolean)

object CertainBenefits {
  implicit val formats = Json.format[CertainBenefits]
}

case class RenewalData(incomeDetails: Option[IncomeDetails],
                       incomeDetailsPY1: Option[IncomeDetails],
                       certainBenefits: Option[CertainBenefits])

object RenewalData {
  implicit val formats = Json.format[RenewalData]
}

case class TcrRenewal(renewalData: RenewalData, applicant2Data: Option[RenewalData],
                      otherIncome:Option[OtherIncome],otherIncomePY1:Option[OtherIncome], hasChangeOfCircs:Boolean) extends Auditable {
  override val txmAuditType = "renewal"

  def applicant2 = applicant2Data match {
    case Some(data) => {
      s"Applicant 2 Income Details=${data.incomeDetails.getOrElse("")}, " +
        s"Applicant 2 Income Details PY1=${data.incomeDetailsPY1.getOrElse("")}, " +
        s"Applicant 2 Certain Benefits=${data.certainBenefits.getOrElse("")}, "
    }
    case None => ""
  }

  override def readableFormat = {
    s"Applicant 1 Income Details=${renewalData.incomeDetails.getOrElse("")}, " +
      s"Applicant 1 Income Details PY1=${renewalData.incomeDetailsPY1.getOrElse("")}, " +
      s"Applicant 1 Certain Benefits=${renewalData.certainBenefits.getOrElse("")}, " +
      applicant2 +
      s"Other Income=${otherIncome.getOrElse("")}, " +
      s"Other Income PY1=${otherIncomePY1.getOrElse("")}, " +
      s"Has Change Of Circs=${hasChangeOfCircs}"
  }
}

object TcrRenewal {
  implicit val formats = Json.format[TcrRenewal]
}

case class D2Check(d2Check: String)

case class ChangeOfCircs(changeOfCircs: Boolean)

object ChangeOfCircs {

  def fromStrings(change: Option[Boolean]) = ChangeOfCircs(change.getOrElse(throw new BadRequestException("User did not answer coc question")))
  def toStrings(change: ChangeOfCircs) = Option(Option(change.changeOfCircs))

  implicit val formats = Json.format[ChangeOfCircs]
}

case class YesOrNo(confirmForm: String)

object YesOrNo {
  implicit val formats = Json.format[YesOrNo]
}
