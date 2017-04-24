Return Claimant Details object
----
  Return claimant details object based on NINO. The service can either return details a single claim or multiple claims.
  Requesting a single claim the 'claims' parameter must not be supplied and the tcrTokenAuth must be supplied in the header.
  Requesting multiple claims, the 'claim' parameter must be supplied with no tcrTokenAuth header.

* **URL**

  `/income/:nino/tax-credits/claimant-details`

* **Method:**
  
  `GET`

*  **URL Params**

  `claims=yes`

   Supplying the claims query parameter will drive the service to return all claims associated from the supplied NINO.


   **Required:**
 
   `nino=[Nino]`
   
   The nino given must be a valid nino. ([http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm](http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm))

*  **HTTP Headers**

   **Required:**
 
   `tcrAuthToken: [TCRAuthToken]` to be supplied when requesting a single claim.

   Note: The tcrAuthToken is only required when the claims query parameter is not supplied.

* **Success Response:**

  * **Code:** 200 <br />
    **Content:**

        [Source...](Please see https://github.com/hmrc/personal-income/blob/master/app/uk/gov/hmrc/apigateway/personalincome/domain/Renewals.scala#L55)

If 'claims' query parameter is not supplied as a query parameter then a single response will be returned based on the barcode reference supplied in the tcrAuthToken.

```json
{
  "hasPartner": false,
  "claimantNumber": 1,
  "renewalFormType": "r",
  "mainApplicantNino": "true",
  "availableForCOCAutomation": false,
  "applicationId": "some-app-id"
}
```

If 'claims' query parameter is supplied, all barcode references associated to the NINO will be returned.
Note the returned 'authenticationToken' attribute is the tcrAuthToken supplied to the claimant-details service.

```json
{
  "references":[
    {
      "household":{
        "barcodeReference":"111111111111111",
        "applicationID":"198765432134566",
        "applicant1":{
          "nino":"CS700100A",
          "title":"Mr",
          "firstForename":"Jon",
          "secondForename":"",
          "surname":"Densmore"
        },
        "householdCeasedDate":"12/10/2010",
        "householdEndReason":"Some reason"
      },
      "renewal":{
        "awardStartDate":"05/04/2016",
        "awardEndDate":"31/08/2016",
        "renewalStatus":"NOT_SUBMITTED",
        "renewalNoticeIssuedDate":"12/10/2030",
        "renewalNoticeFirstSpecifiedDate":"12/10/2010"
      }
    },
    {
      "household":{
        "barcodeReference":"222222222222222",
        "applicationID":"198765432134567",
        "applicant1":{
          "nino":"CS700100A",
          "title":"Mr",
          "firstForename":"Jon",
          "secondForename":"",
          "surname":"Densmore"
        },
        "householdCeasedDate":"12/10/2010",
        "householdEndReason":"Some reason"
      },
      "renewal":{
        "awardStartDate":"31/08/2016",
        "awardEndDate":"31/12/2016",
        "renewalStatus":"NOT_SUBMITTED",
        "renewalNoticeIssuedDate":"12/10/2030",
        "renewalNoticeFirstSpecifiedDate":"12/10/2010"
      }
    },
    {
      "household":{
        "barcodeReference":"200000000000014",
        "applicationID":"198765432134567",
        "applicant1":{
          "nino":"AM242413B",
          "title":"Miss",
          "firstForename":"Hazel",
          "secondForename":"",
          "surname":"Young"
        },
        "applicant2":{
          "nino":"CS700100A",
          "title":"Mr",
          "firstForename":"Jon",
          "secondForename":"",
          "surname":"Densmore"
        }
      },
      "renewal":{
        "awardStartDate":"31/12/2016",
        "awardEndDate":"31/07/2017",
        "renewalStatus":"NOT_SUBMITTED",
        "renewalNoticeIssuedDate":"12/10/2030",
        "renewalNoticeFirstSpecifiedDate":"12/10/2010"
      }
    }
  ]
}
```


* **Error Response:**

  * **Code:** 400 BADREQUEST <br />
    **Content:** `{"code":"BADREQUEST","message":"Bad Request"}`

  * **Code:** 404 BADREQUEST <br />
    **Content:** `{"code":"NOT_FOUND","message":"Not Found"}`

  * **Code:** 401 UNAUTHORIZED <br />
    **Content:** `{"code":"UNAUTHORIZED","message":"Bearer token is missing or not authorized for access"}`

  * **Code:** 403 FORBIDDEN <br />
    **Content:** `{"code":"FORBIDDEN","message":"No auth header supplied in http request"}`

  * **Code:** 406 NOT ACCEPTABLE <br />
    **Content:** `{"code":"ACCEPT_HEADER_INVALID","message":"The accept header is missing or invalid"}`

  OR

  * **Code:** 500 INTERNAL_SERVER_ERROR <br />


