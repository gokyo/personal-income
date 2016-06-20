First step in Renewal process
----
  Validates the NINO and renewal reference, returning the TCR auth-token.
  
* **URL**

  `/income/:nino/tax-credits/:renewalReference/auth`

* **Method:**
  
  `GET`
  
*  **URL Params**

   **Required:**
 
   `nino=[Nino]`
   
   The nino given must be a valid nino. ([http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm](http://www.hmrc.gov.uk/manuals/nimmanual/nim39110.htm))

   `renewalReference=[String]`

* **Success Response:**

  * **Code:** 200 <br />
    **Content:** 

        [Source...](https://github.com/hmrc/personal-income/blob/master/app/uk/gov/hmrc/apigateway/personalincome/domain/Renewals.scala#L33)

```json
{
  "tcrAuthToken": "some-token"
}
```
 
* **Error Response:**

  * **Code:** 400 BADREQUEST <br />
    **Content:** `{"code":"BADREQUEST","message":"Bad Request"}`

  * **Code:** 401 UNAUTHORIZED <br />
    **Content:** `{"code":"UNAUTHORIZED","message":"Bearer token is missing or not authorized for access"}`

  * **Code:** 404 NOT_FOUND <br />

  * **Code:** 406 NOT ACCEPTABLE <br />
    **Content:** `{"code":"ACCEPT_HEADER_INVALID","message":"The accept header is missing or invalid"}`

  OR when a user does not exist

  * **Code:** 500 INTERNAL_SERVER_ERROR <br />


