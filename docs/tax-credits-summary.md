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
      "paymentSeq": [
        {
          "amount": 55,
          "paymentDate": 1509008158781,
          "oneOffPayment": false,
          "earlyPayment": false
        },
        {
          "amount": 55,
          "paymentDate": 1511690158781,
          "oneOffPayment": false,
          "holidayType": "bankHoliday",
          "earlyPayment": true,
          "explanatoryText" : "Your payment is early because of UK bank holidays."
        },
        {
          "amount": 55,
          "paymentDate": 1514282158781,
          "oneOffPayment": true,
          "earlyPayment": false,
          "explanatoryText" : "This is because of a recent change and is to help you get the right amount of tax credits."
        }
      ],
      "paymentFrequency": "weekly",
      "previousPaymentSeq": [
        {
          "amount": 33,
          "paymentDate": 1503737758781,
          "oneOffPayment": false,
          "earlyPayment": false
        },
        {
          "amount": 43,
          "paymentDate": 1501059358781,
          "oneOffPayment": false,
          "holidayType": "bankHoliday",
          "earlyPayment": true,
          "explanatoryText" : "Your payment was early because of UK bank holidays."
        },
        {
          "amount": 53,
          "paymentDate": 1498467358781,
          "oneOffPayment": true,
          "earlyPayment": false,
          "explanatoryText" : "This was because of a recent change and was to help you get the right amount of tax credits."
        }
      ]
    },
    "childTaxCredit": {
      "paymentSeq": [
        {
          "amount": 55,
          "paymentDate": 1509008158781,
          "oneOffPayment": false,
          "earlyPayment": false
        },
        {
          "amount": 55,
          "paymentDate": 1511690158781,
          "oneOffPayment": false,
          "holidayType": "bankHoliday",
          "earlyPayment": true,
          "explanatoryText" : "Your payment is early because of UK bank holidays."
        },
        {
          "amount": 55,
          "paymentDate": 1514282158781,
          "oneOffPayment": true,
          "earlyPayment": false,
          "explanatoryText" : "This is because of a recent change and is to help you get the right amount of tax credits."
        }
      ],
      "paymentFrequency": "weekly"
    },
    "paymentEnabled": true,
    "totalsByDate": [
      {
        "amount": 110,
        "paymentDate": 1509008158781
      },
      {
        "amount": 110,
        "paymentDate": 1511690158781
      },
      {
        "amount": 110,
        "paymentDate": 1514282158781
      }
    ],
    "previousTotalsByDate": [
      {
        "amount": 53,
        "paymentDate": 1498467358781
      },
      {
        "amount": 43,
        "paymentDate": 1501059358781
      },
      {
        "amount": 33,
        "paymentDate": 1503737758781
      }
    ]
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



