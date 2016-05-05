Return Claiment Details object
----
  Return claiment details object based on NINO.

* **URL**

  `/income/:nino/tax-credits/claimant-details`

* **Method:**
  
  `GET`
  
*  **URL Params**

   **Required:**
 
   `nino=[Nino]`
   
   The nino given must be a valid nino. ([http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm](http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm))

*  **HTTP Headers**

   **Required:**
 
   `tcrAuthToken: Basic [TCRAuthToken]`

* **Success Response:**

  * **Code:** 200 <br />
    **Content:**

        [Source...](Please see https://github.com/hmrc/personal-income/blob/master/app/uk/gov/hmrc/apigateway/personalincome/domain/Renewals.scala#L55)

```json
{
  "hasPartner": false,
  "claimantNumber": 1,
  "renewalFormType": "renewalForm",
  "mainApplicantNino": "CS700100A",
  "availableForCOCAutomation": false,
  "applicationId": "some-app-id"
}
```

* **Error Response:**

  * **Code:** 400 BADREQUEST <br />
    **Content:** `{"code":"BADREQUEST","message":"Bad Request"}`

  * **Code:** 401 UNAUTHORIZED <br />
    **Content:** `{"code":"UNAUTHORIZED","message":"Bearer token is missing or not authorized for access"}`

  * **Code:** 403 FORBIDDEN <br />
    **Content:** `{"code":"FORBIDDEN","message":"No auth header supplied in http request"}`

  * **Code:** 406 NOT ACCEPTABLE <br />
    **Content:** `{"code":"ACCEPT_HEADER_INVALID","message":"The accept header is missing or invalid"}`

  OR

  * **Code:** 500 INTERNAL_SERVER_ERROR <br />


