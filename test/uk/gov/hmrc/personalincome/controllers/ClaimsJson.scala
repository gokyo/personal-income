/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.personalincome.controllers

trait ClaimsJson {




  val claimsJson = """{
                     |  "references": [
                     |    {
                     |      "household": {
                     |        "barcodeReference": "000000000000000",
                     |        "applicationID": "198765432134566",
                     |        "applicant1": {
                     |          "nino": "CS700100A",
                     |          "title": "Mr",
                     |          "firstForename": "Jon",
                     |          "secondForename": "",
                     |          "surname": "Densmore"
                     |        },
                     |        "householdCeasedDate": "20101012",
                     |        "householdEndReason": "Some reason"
                     |      },
                     |      "renewal": {
                     |        "awardStartDate": "2016-04-05",
                     |        "awardEndDate": "2016-08-31",
                     |        "renewalNoticeIssuedDate": "20301012",
                     |        "renewalNoticeFirstSpecifiedDate": "20101012"
                     |      }
                     |    },
                     |    {
                     |      "household": {
                     |        "barcodeReference": "111111111111111",
                     |        "applicationID": "198765432134566",
                     |        "applicant1": {
                     |          "nino": "CS700100A",
                     |          "title": "Mr",
                     |          "firstForename": "Jon",
                     |          "secondForename": "",
                     |          "surname": "Densmore"
                     |        },
                     |        "householdCeasedDate": "20101012",
                     |        "householdEndReason": "Some reason"
                     |      },
                     |      "renewal": {
                     |        "awardStartDate": "2016-04-05",
                     |        "awardEndDate": "2016-08-31",
                     |        "renewalNoticeIssuedDate": "20301012",
                     |        "renewalNoticeFirstSpecifiedDate": "20101012"
                     |      }
                     |    },
                     |    {
                     |      "household": {
                     |        "barcodeReference": "222222222222222",
                     |        "applicationID": "198765432134567",
                     |        "applicant1": {
                     |          "nino": "CS700100A",
                     |          "title": "Mr",
                     |          "firstForename": "Jon",
                     |          "secondForename": "",
                     |          "surname": "Densmore"
                     |        },
                     |        "householdCeasedDate": "20101012",
                     |        "householdEndReason": "Some reason"
                     |      },
                     |      "renewal": {
                     |        "awardStartDate": "2016-08-31",
                     |        "awardEndDate": "2016-12-31",
                     |        "renewalStatus": "PARTIAL CAPTURE",
                     |        "renewalNoticeIssuedDate": "20301012",
                     |        "renewalNoticeFirstSpecifiedDate": "20101012"
                     |      }
                     |    },
                     |    {
                     |      "household": {
                     |        "barcodeReference": "333333333333333",
                     |        "applicationID": "198765432134568",
                     |        "applicant1": {
                     |          "nino": "AM242413B",
                     |          "title": "Miss",
                     |          "firstForename": "Hazel",
                     |          "secondForename": "",
                     |          "surname": "Young"
                     |        },
                     |        "applicant2": {
                     |          "nino": "AP412713B",
                     |          "title": "Miss",
                     |          "firstForename": "Cathy",
                     |          "secondForename": "",
                     |          "surname": "Garcia-Vazquez"
                     |        }
                     |      },
                     |      "renewal": {
                     |        "awardStartDate": "2016-12-31",
                     |        "awardEndDate": "2017-07-31",
                     |        "renewalStatus": "AWAITING PROCESS",
                     |        "renewalNoticeIssuedDate": "20301012",
                     |        "renewalNoticeFirstSpecifiedDate": "20101012"
                     |      }
                     |    },
                     |    {
                     |      "household": {
                     |        "barcodeReference": "200000000000014",
                     |        "applicationID": "198765432134567",
                     |        "applicant1": {
                     |          "nino": "AM242413B",
                     |          "title": "Miss",
                     |          "firstForename": "Hazel",
                     |          "secondForename": "",
                     |          "surname": "Young"
                     |        },
                     |        "applicant2": {
                     |          "nino": "CS700100A",
                     |          "title": "Mr",
                     |          "firstForename": "Jon",
                     |          "secondForename": "",
                     |          "surname": "Densmore"
                     |        }
                     |      },
                     |      "renewal": {
                     |        "awardStartDate": "2016-12-31",
                     |        "awardEndDate": "2017-07-31",
                     |        "renewalStatus": "REPLY USED FOR FINALISATION",
                     |        "renewalNoticeIssuedDate": "20301012",
                     |        "renewalNoticeFirstSpecifiedDate": "20101012"
                     |      }
                     |    }
                     |  ]
                     |}""".stripMargin

  val matchedClaimsJson = """{
                            |  "references": [
                            |    {
                            |      "household": {
                            |        "barcodeReference": "000000000000000",
                            |        "applicationID": "198765432134566",
                            |        "applicant1": {
                            |          "nino": "CS700100A",
                            |          "title": "Mr",
                            |          "firstForename": "Jon",
                            |          "secondForename": "",
                            |          "surname": "Densmore"
                            |        },
                            |        "householdCeasedDate": "12/10/2010",
                            |        "householdEndReason": "Some reason"
                            |      },
                            |      "renewal": {
                            |        "awardStartDate": "05/04/2016",
                            |        "awardEndDate": "31/08/2016",
                            |        "renewalStatus": "AWAITING_BARCODE",
                            |        "renewalNoticeIssuedDate": "12/10/2030",
                            |        "renewalNoticeFirstSpecifiedDate": "12/10/2010"
                            |      }
                            |    },
                            |    {
                            |      "household": {
                            |        "barcodeReference": "111111111111111",
                            |        "applicationID": "198765432134566",
                            |        "applicant1": {
                            |          "nino": "CS700100A",
                            |          "title": "Mr",
                            |          "firstForename": "Jon",
                            |          "secondForename": "",
                            |          "surname": "Densmore"
                            |        },
                            |        "householdCeasedDate": "12/10/2010",
                            |        "householdEndReason": "Some reason"
                            |      },
                            |      "renewal": {
                            |        "awardStartDate": "05/04/2016",
                            |        "awardEndDate": "31/08/2016",
                            |        "renewalStatus": "NOT_SUBMITTED",
                            |        "renewalNoticeIssuedDate": "12/10/2030",
                            |        "renewalNoticeFirstSpecifiedDate": "12/10/2010"
                            |      }
                            |    },
                            |    {
                            |      "household": {
                            |        "barcodeReference": "222222222222222",
                            |        "applicationID": "198765432134567",
                            |        "applicant1": {
                            |          "nino": "CS700100A",
                            |          "title": "Mr",
                            |          "firstForename": "Jon",
                            |          "secondForename": "",
                            |          "surname": "Densmore"
                            |        },
                            |        "householdCeasedDate": "12/10/2010",
                            |        "householdEndReason": "Some reason"
                            |      },
                            |      "renewal": {
                            |        "awardStartDate": "31/08/2016",
                            |        "awardEndDate": "31/12/2016",
                            |        "renewalStatus": "SUBMITTED_AND_PROCESSING",
                            |        "renewalNoticeIssuedDate": "12/10/2030",
                            |        "renewalNoticeFirstSpecifiedDate": "12/10/2010"
                            |      }
                            |    },
                            |    {
                            |      "household": {
                            |        "barcodeReference": "200000000000014",
                            |        "applicationID": "198765432134567",
                            |        "applicant1": {
                            |          "nino": "AM242413B",
                            |          "title": "Miss",
                            |          "firstForename": "Hazel",
                            |          "secondForename": "",
                            |          "surname": "Young"
                            |        },
                            |        "applicant2": {
                            |          "nino": "CS700100A",
                            |          "title": "Mr",
                            |          "firstForename": "Jon",
                            |          "secondForename": "",
                            |          "surname": "Densmore"
                            |        }
                            |      },
                            |      "renewal": {
                            |        "awardStartDate": "31/12/2016",
                            |        "awardEndDate": "31/07/2017",
                            |        "renewalStatus": "COMPLETE",
                            |        "renewalNoticeIssuedDate": "12/10/2030",
                            |        "renewalNoticeFirstSpecifiedDate": "12/10/2010"
                            |      }
                            |    }
                            |  ]
                            |}""".stripMargin




  // TODO...

  val claimsJsonWithInvalidDates = """{
                     |  "references": [
                     |    {
                     |      "household": {
                     |        "barcodeReference": "000000000000000",
                     |        "applicationID": "198765432134566",
                     |        "applicant1": {
                     |          "nino": "CS700100A",
                     |          "title": "Mr",
                     |          "firstForename": "Jon",
                     |          "secondForename": "",
                     |          "surname": "Densmore"
                     |        },
                     |        "householdCeasedDate": "",
                     |        "householdEndReason": "Some reason"
                     |      },
                     |      "renewal": {
                     |        "awardStartDate": "2016-04-05",
                     |        "awardEndDate": "2016-08-31-invalid",
                     |        "renewalNoticeIssuedDate": "20301012",
                     |        "renewalNoticeFirstSpecifiedDate": "20101012"
                     |      }
                     |    }
                     |  ]
                     |}""".stripMargin

  val matchedClaimsJsonWithInvalidDates = """{
                            |  "references": [
                            |    {
                            |      "household": {
                            |        "barcodeReference": "000000000000000",
                            |        "applicationID": "198765432134566",
                            |        "applicant1": {
                            |          "nino": "CS700100A",
                            |          "title": "Mr",
                            |          "firstForename": "Jon",
                            |          "secondForename": "",
                            |          "surname": "Densmore"
                            |        },
                            |        "householdEndReason": "Some reason"
                            |      },
                            |      "renewal": {
                            |        "awardStartDate": "05/04/2016",
                            |        "renewalStatus": "AWAITING_BARCODE",
                            |        "renewalNoticeIssuedDate": "12/10/2030",
                            |        "renewalNoticeFirstSpecifiedDate": "12/10/2010"
                            |      }
                            |    }
                            |  ]
                            |}""".stripMargin

}
