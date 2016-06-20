The Tax Summary object
----
  Fetch the Tax Summary object for a given nino and year.
  
* **URL**

  `/income/:nino/tax-summary/:taxYear`

* **Method:**
  
  `GET`
  
*  **URL Params**

   **Required:**
 
   `nino=[Nino]`
   
   The nino given must be a valid nino. ([http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm](http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm))

   `taxYear=Int`

* **Success Response:**

  * **Code:** 200 <br />
    **Content:** 

        [Source...](https://github.com/hmrc/personal-income/blob/master/app/uk/gov/hmrc/apigateway/personalincome/domain/TaxSummaryModel.scala#L389)

```json
{
  "nino": "ZX659315",
  "version": 1,
  "increasesTax": {
    "incomes": {
      "employments": {
        "total" : 20000,
        "noneTaxCodeIncomes": {
            "totalIncome": 0
        },
        "taxCodeIncomes": {
            "totalIncome": 20000,
            "totalTax": 3158,
            "totalTaxableIncome": 15790,
            "taxCodeIncomes": [
              {
                "name": "PAYESCHEMEOPERATORNAME70244",
                "taxCode": "420L",
                "employmentId": 5,
                "employmentPayeRef": "ZZ53186",
                "employmentType": 1,
                "incomeType": 0,
                "employmentStatus": 1,
                "tax": {
                  "totalIncome": 20000,
                  "totalTaxableIncome": 15790,
                  "totalTax": 3158,
                  "taxBands": [
                    {
                      "income": 15790,
                      "tax": 3158,
                      "lowerBand": 0,
                      "upperBand": 31865,
                      "rate": 20
                    }
                  ]
                }
              }
            }
        ]
      }
    },
    "benefitsFromEmployment": {
      "amount": 5800,
      "componentType": 0,
      "description": "",
      "iabdSummaries": [
        {
          "iabdType": 53,
          "description": "Travel and Subsistence",
          "amount": 5000,
          "employmentId": 5,
          "employmentName": "PAYESCHEMEOPERATORNAME70244"
        },
        {
          "iabdType": 32,
          "description": "Telephone",
          "amount": 800,
          "employmentId": 5,
          "employmentName": "PAYESCHEMEOPERATORNAME70244"
        }
      ]
    }
  },
  "decreasesTax": {
    "personalAllowance": 10000,
    "personalAllowanceSourceAmount": 10000
  },
  "totalLiability": {
    "nonSavings": {
      "totalIncome": 25800,
      "totalTaxableIncome": 15800,
      "totalTax": 3160,
      "taxBands": [
        {
          "income": 15800,
          "tax": 3160,
          "lowerBand": 0,
          "upperBand": 31865,
          "rate": 20
        }
      ]
    },
    "totalLiability": 3160
  }
}
```
 
* **Error Response:**

  * **Code:** 400 BADREQUEST <br />
    **Content:** `{"code":"BADREQUEST","message":"Bad Request"}`

  * **Code:** 401 UNAUTHORIZED <br/>
    **Content:** `{"code":"UNAUTHORIZED","message":"Bearer token is missing or not authorized for access"}`

  * **Code:** 404 NOTFOUND <br/>

  * **Code:** 406 NOT ACCEPTABLE <br />
    **Content:** `{"code":"ACCEPT_HEADER_INVALID","message":"The accept header is missing or invalid"}`

  OR when a user does not exist or server failure

  * **Code:** 500 INTERNAL_SERVER_ERROR <br/>



