The Tax Credits Summary object
----
  Fetch the Tax Credits Summary object for a given nino.
  
* **URL**

  `/income/:nino/tax-credits-summary`

* **Method:**
  
  `GET`
  
*  **URL Params**

   **Required:**
 
   `nino=[Nino]`
   
   The nino given must be a valid nino. ([http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm](http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm))

* **Success Response:**

  * **Code:** 200 <br />
    **Content:** 

        [Source...](https://github.com/hmrc/personal-income/blob/master/app/uk/gov/hmrc/apigateway/personalincome/domain/TaxSummaryModel.scala#L389)

```json
{
  "paymentSummary": {
    "workingTaxCredit": {
      "amount": 160.34,
      "paymentDate": 1437004800000,
      "paymentFrequency": "WEEKLY"
    },
    "childTaxCredit": {
      "amount": 140.12,
      "paymentDate": 1437004800000,
      "paymentFrequency": "WEEKLY"
    }
  },
  "personalDetails": {
    "forename": "firstname",
    "surname": "surname",
    "nino": "CS700100A",
    "address": {
      "addressLine1": "addressLine1",
      "addressLine2": "addressLine2",
      "addressLine3": "addressLine3",
      "addressLine4": "addressLine4",
      "postCode": "postcode"
    }
  },
  "partnerDetails": {
    "forename": "forename",
    "otherForenames": "othernames",
    "surname": "surname",
    "nino": "CS700100A",
    "address": {
      "addressLine1": "addressLine1",
      "addressLine2": "addressLine2",
      "addressLine3": "addressLine3",
      "addressLine4": "addressLine4",
      "postCode": "postcode"
    }
  },
  "children": {
    "child": [
      {
        "firstNames": "Sarah",
        "surname": "Smith",
        "dateOfBirth": 936057600000,
        "hasFTNAE": false,
        "hasConnexions": false,
        "isActive": true
      },
      {
        "firstNames": "Joseph",
        "surname": "Smith",
        "dateOfBirth": 884304000000,
        "hasFTNAE": false,
        "hasConnexions": false,
        "isActive": true
      },
      {
        "firstNames": "Mary",
        "surname": "Smith",
        "dateOfBirth": 852768000000,
        "hasFTNAE": false,
        "hasConnexions": false,
        "isActive": true
      }
    ]
  }
}
```
 
* **Error Response:**

  * **Code:** 400 BADREQUEST <br />
    **Content:** `{"code":"BADREQUEST","message":"Bad Request"}`

  * **Code:** 401 UNAUTHORIZED <br/>
    **Content:** `{"code":"UNAUTHORIZED","message":"Bearer token is missing or not authorized for access"}`

  * **Code:** 404 NOTFOUND <br/>
    **Content:** `{ "code" : "MATCHING_RESOURCE_NOT_FOUND", "message" : "A resource with the name in the request can not be found in the API" }`

  * **Code:** 406 NOT ACCEPTABLE <br />
    **Content:** `{"code":"ACCEPT_HEADER_INVALID","message":"The accept header is missing or invalid"}`

  OR when a user does not exist or server failure

  * **Code:** 500 INTERNAL_SERVER_ERROR <br/>



