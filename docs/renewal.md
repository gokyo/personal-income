Submit renewal
----
  Submit the renewal for off-line processing.

* **URL**

  `/income/:nino/tax-credits/renewal`

* **Method:**

  `POST`

    Example JSON post payload for renewal.

```json
{
  "renewalData": {
    "incomeDetails": {
      "taxableBenefits": 10,
      "earnings": 20,
      "companyBenefits": 30,
      "seProfits": 40,
      "seProfitsEstimated": true
    },
    "incomeDetailsPY1": {
      "taxableBenefits": 10,
      "earnings": 20,
      "companyBenefits": 30,
      "seProfits": 40,
      "seProfitsEstimated": true
    },
    "certainBenefits": {
      "receivedBenefits": false,
      "incomeSupport": false,
      "jsa": false,
      "esa": false,
      "pensionCredit": false
    }
  },
  "otherIncome": {
    "otherHouseholdIncome": 100,
    "isOtherHouseholdIncomeEst": false
  },
  "otherIncomePY1": {
    "otherHouseholdIncome": 100,
    "isOtherHouseholdIncomeEst": false
  },
  "hasChangeOfCircs": false
}
```


*  **URL Params**

   **Required:**
 
   `nino=[Nino]`
   
   The nino given must be a valid nino. ([http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm](http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm))

*  **HTTP Headers**

   **Required:**
 
   `tcrAuthToken: Basic [TCRAuthToken]`


* **Success Response:**

  * **Code:** 200 <br />

* **Error Response:**

  * **Code:** 400 BADREQUEST <br />
    **Content:** `{"code":"BADREQUEST","message":"Bad Request"}`

  * **Code:** 401 UNAUTHORIZED <br />
    **Content:** `{"code":"UNAUTHORIZED","message":"Bearer token is missing or not authorized for access"}`

  * **Code:** 403 FORBIDDEN <br />
    **Content:** `{"code":"FORBIDDEN","message":"No auth header supplied in http request"}`

  * **Code:** 406 NOT ACCEPTABLE <br />
    **Content:** `{"code":"ACCEPT_HEADER_INVALID","message":"The accept header is missing or invalid"}`

  OR when the details cannot be resolved.

  * **Code:** 500 INTERNAL_SERVER_ERROR <br />


