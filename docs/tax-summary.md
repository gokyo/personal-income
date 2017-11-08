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
  "baseViewModel": {
    "estimatedIncomeTax": 48244,
    "hasChanges": false,
    "hasTamc": true,
    "personalAllowance": 0,
    "simpleTaxUser": false,
    "taxCodesList": [
      "K5817",
      "D0"
    ],
    "taxFree": 2260,
    "taxableIncome": 145435
  },
  "estimatedIncomeWrapper": {
    "estimatedIncome": {
      "additionalTaxTable": [
        {
          "amount": 147,
          "description": "Underpayment from previous years"
        },
        {
          "amount": 1500,
          "description": "Child Benefit"
        },
        {
          "amount": 169,
          "description": "Outstanding debt from previous year"
        }
      ],
      "additionalTaxTableTotal": 1816,
      "graph": {
        "bands": [
          {
            "bandType": "pa",
            "barPercentage": 0,
            "colour": "",
            "income": 2260,
            "tablePercentage": "0",
            "tax": 0
          },
          {
            "bandType": "NA",
            "barPercentage": 0,
            "colour": "",
            "income": 32010,
            "tablePercentage": "20",
            "tax": 6402
          },
          {
            "bandType": "NA",
            "barPercentage": 0,
            "colour": "",
            "income": 20000,
            "tablePercentage": "32.5",
            "tax": 6500
          },
          {
            "bandType": "NA",
            "barPercentage": 0,
            "colour": "",
            "income": 81165,
            "tablePercentage": "40",
            "tax": 32466
          },
          {
            "bandType": "NA",
            "barPercentage": 0,
            "colour": "",
            "income": 5000,
            "tablePercentage": "40",
            "tax": 2000
          },
          {
            "bandType": "NA",
            "barPercentage": 0,
            "colour": "",
            "income": 5000,
            "tablePercentage": "40",
            "tax": 2000
          }
        ],
        "id": "taxGraph",
        "incomeAsPercentage": 0,
        "incomeTotal": 145435,
        "minBand": 0,
        "nextBand": 0,
        "taxTotal": 49368,
        "zeroIncomeAsPercentage": 0,
        "zeroIncomeTotal": 0
      },
      "hasChanges": false,
      "hasPSA": false,
      "hasSSR": false,
      "incomeEstimate": 145435,
      "incomeTaxEstimate": 48244,
      "increasesTax": true,
      "nextYearTaxTotal": 0,
      "potentialUnderpayment": false,
      "reductionsTable": [
        {
          "additionalInfo": "Tax on other income not collected through your tax code, for example tax paid through Self Assessment.",
          "amount": -40,
          "description": "Tax on other income"
        },
        {
          "additionalInfo": "Interest from company dividends is taxed at the dividend ordinary rate (10%) before it is paid to you.",
          "amount": -2000,
          "description": "Tax on dividends"
        },
        {
          "additionalInfo": "This is the interest that has been taxed by the basic rate of 20% by your bank, building society or financial institution before it is paid to you.",
          "amount": -1000,
          "description": "Tax on interest from savings and investments"
        }
      ],
      "reductionsTableTotal": -3040,
      "taxBands": [
        {
          "income": 20000,
          "lowerBand": 32010,
          "rate": 32.5,
          "tax": 6500,
          "upperBand": 150225
        }
      ],
      "taxCodes": [
        "K5817",
        "D0"
      ],
      "taxFreeEstimate": 2260,
      "taxRelief": true,
      "ukDividends": {
        "amount": 20000,
        "componentType": 0,
        "description": "",
        "iabdSummaries": [
          {
            "amount": 20000,
            "description": "UK Dividend",
            "employmentId": 0,
            "iabdType": 76
          }
        ]
      }
    }
  },
  "taxSummaryDetails": {
    "decreasesTax": {
      "blindPerson": {
        "amount": 200,
        "componentType": 0,
        "description": "",
        "iabdSummaries": [
          {
            "amount": 100,
            "description": "BPA Received from Spouse/Civil Partner",
            "employmentId": 0,
            "iabdType": 15
          },
          {
            "amount": 100,
            "description": "Blind Persons Allowance",
            "employmentId": 0,
            "iabdType": 14
          }
        ]
      },
      "expenses": {
        "amount": 400,
        "componentType": 0,
        "description": "",
        "iabdSummaries": [
          {
            "amount": 100,
            "description": "Early Years Adjustment",
            "employmentId": 0,
            "iabdType": 101
          },
          {
            "amount": 100,
            "description": "Professional Subscriptions",
            "employmentId": 0,
            "iabdType": 57
          },
          {
            "amount": 100,
            "description": "Flat Rate Job Expenses",
            "employmentId": 0,
            "iabdType": 56
          },
          {
            "amount": 100,
            "description": "Flat Rate Job Expenses",
            "employmentId": 0,
            "iabdType": 56
          }
        ]
      },
      "giftRelated": {
        "amount": 100,
        "componentType": 0,
        "description": "",
        "iabdSummaries": [
          {
            "amount": 100,
            "description": "Gifts of Shares to Charity",
            "employmentId": 0,
            "iabdType": 17
          }
        ]
      },
      "jobExpenses": {
        "amount": 500,
        "componentType": 0,
        "description": "",
        "iabdSummaries": [
          {
            "amount": 100,
            "description": "Mileage Allowance Relief",
            "employmentId": 0,
            "iabdType": 61
          },
          {
            "amount": 100,
            "description": "Vehicle Expenses",
            "employmentId": 0,
            "iabdType": 60
          },
          {
            "amount": 100,
            "description": "Other Expenses",
            "employmentId": 0,
            "iabdType": 59
          },
          {
            "amount": 100,
            "description": "Hotel and Meal Expenses",
            "employmentId": 0,
            "iabdType": 58
          },
          {
            "amount": 100,
            "description": "Job Expenses",
            "employmentId": 0,
            "iabdType": 55
          }
        ]
      },
      "paReceivedAmount": 1060,
      "paTapered": true,
      "personalAllowance": 0,
      "personalAllowanceSourceAmount": 10600,
      "total": 2260
    },
    "extensionReliefs": {
      "giftAid": {
        "reliefAmount": 25,
        "sourceAmount": 100
      },
      "personalPension": {
        "reliefAmount": 20,
        "sourceAmount": 100
      }
    },
    "increasesTax": {
      "benefitsFromEmployment": {
        "amount": 14835,
        "componentType": 0,
        "description": "",
        "iabdSummaries": [
          {
            "amount": 12000,
            "description": "Travel and Subsistence",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 53
          },
          {
            "amount": 100,
            "description": "Vouchers and Credit Cards",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 54
          },
          {
            "amount": 100,
            "description": "Travel and Subsistence",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 53
          },
          {
            "amount": 100,
            "description": "Qualfying Relocation Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 50
          },
          {
            "amount": 100,
            "description": "Personal Incidental Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 49
          },
          {
            "amount": 100,
            "description": "Payments on Employee's Behalf",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 48
          },
          {
            "amount": 100,
            "description": "Other Items",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 47
          },
          {
            "amount": 100,
            "description": "Nursery Places",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 46
          },
          {
            "amount": 100,
            "description": "Non-qualifying Relocation Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 45
          },
          {
            "amount": 100,
            "description": "Mileage",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 44
          },
          {
            "amount": 100,
            "description": "Income Tax Paid but not deducted from Director's Remuneration",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 52
          },
          {
            "amount": 100,
            "description": "Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 43
          },
          {
            "amount": 100,
            "description": "Entertaining",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 42
          },
          {
            "amount": 100,
            "description": "Employer Provided Services",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 8
          },
          {
            "amount": 100,
            "description": "Employer Provided Professional Subscription",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 51
          },
          {
            "amount": 100,
            "description": "Educational Services",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 41
          },
          {
            "amount": 100,
            "description": "Asset Transfer",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 40
          },
          {
            "amount": 100,
            "description": "Assets",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 39
          },
          {
            "amount": 100,
            "description": "Accommodation",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 38
          },
          {
            "amount": 100,
            "description": "Non-Cash Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 117
          },
          {
            "amount": 100,
            "description": "Beneficial Loan",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 37
          },
          {
            "amount": 100,
            "description": "Van Fuel Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 36
          },
          {
            "amount": 100,
            "description": "Van Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 35
          },
          {
            "amount": 100,
            "description": "Taxable Expenses Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 34
          },
          {
            "amount": 100,
            "description": "Service Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 33
          },
          {
            "amount": 100,
            "description": "Telephone",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 32
          },
          {
            "amount": 100,
            "description": "Car Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 31
          },
          {
            "amount": 100,
            "description": "Medical Insurance",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 30
          },
          {
            "amount": 100,
            "description": "Car Fuel Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 29
          },
          {
            "amount": 35,
            "description": "Vouchers and Credit Cards",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 54
          }
        ]
      },
      "incomes": {
        "noneTaxCodeIncomes": {
          "bankBsInterest": {
            "amount": 5000,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 5000,
                "description": "Bank / Building Society Interest",
                "employmentId": 0,
                "iabdType": 75
              }
            ]
          },
          "dividends": {
            "amount": 20000,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 20000,
                "description": "UK Dividend",
                "employmentId": 0,
                "iabdType": 76
              }
            ]
          },
          "otherIncome": {
            "amount": 200,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 100,
                "description": "Profit",
                "employmentId": 0,
                "iabdType": 72
              },
              {
                "amount": 100,
                "description": "Non-Coded Income",
                "employmentId": 0,
                "iabdType": 19
              }
            ]
          },
          "statePension": 100,
          "taxableStateBenefit": {
            "amount": 300,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 100,
                "description": "Employment and Support Allowance",
                "employmentId": 0,
                "iabdType": 123
              },
              {
                "amount": 100,
                "description": "Job Seekers Allowance",
                "employmentId": 0,
                "iabdType": 84
              },
              {
                "amount": 100,
                "description": "Incapacity Benefit",
                "employmentId": 0,
                "iabdType": 83
              }
            ]
          },
          "totalIncome": 30600,
          "untaxedInterest": {
            "amount": 5000,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 5000,
                "description": "Untaxed Interest",
                "employmentId": 0,
                "iabdType": 82
              }
            ]
          }
        },
        "taxCodeIncomes": {
          "employments": {
            "taxCodeIncomes": [
              {
                "employmentId": 1,
                "employmentPayeRef": "BT456",
                "employmentStatus": 1,
                "employmentType": 1,
                "income": 50000,
                "incomeType": 0,
                "isEditable": true,
                "isLive": true,
                "isOccupationalPension": false,
                "isPrimary": true,
                "jobTitle": "Job Title 1",
                "name": "Test Employer",
                "startDate": "2010-01-01",
                "tax": {
                  "actualTaxDueAssumingBasicRateAlreadyPaid": 26949.8,
                  "allowReliefDeducts": -58267,
                  "taxBands": [
                    {
                      "income": 31785,
                      "lowerBand": 0,
                      "rate": 20,
                      "tax": 6357,
                      "upperBand": 31785
                    },
                    {
                      "income": 76482,
                      "lowerBand": 31785,
                      "rate": 40,
                      "tax": 30592.8,
                      "upperBand": 150000
                    },
                    {
                      "income": 0,
                      "lowerBand": 150000,
                      "rate": 45,
                      "tax": 0
                    }
                  ],
                  "totalIncome": 50000,
                  "totalTax": 36949.8,
                  "totalTaxableIncome": 108267
                },
                "taxCode": "K5817",
                "worksNumber": "WN 1"
              }
            ],
            "totalIncome": 50000,
            "totalTax": 36949.8,
            "totalTaxableIncome": 108267
          },
          "hasDuplicateEmploymentNames": false,
          "occupationalPensions": {
            "taxCodeIncomes": [
              {
                "employmentId": 2,
                "employmentPayeRef": "AS456",
                "employmentStatus": 1,
                "employmentType": 2,
                "income": 50000,
                "incomeType": 1,
                "isEditable": true,
                "isLive": true,
                "isOccupationalPension": true,
                "isPrimary": false,
                "jobTitle": "Job Title 2",
                "name": "Test Pension Provider",
                "startDate": "2010-01-01",
                "tax": {
                  "actualTaxDueAssumingBasicRateAlreadyPaid": 11273.36,
                  "totalIncome": 50000,
                  "totalTax": 11273.36,
                  "totalTaxableIncome": 50000
                },
                "taxCode": "D0",
                "worksNumber": "WN 2"
              }
            ],
            "totalIncome": 50000,
            "totalTax": 11273.36,
            "totalTaxableIncome": 50000
          },
          "totalIncome": 100000,
          "totalTax": 48223.16,
          "totalTaxableIncome": 158267
        },
        "total": 130600
      },
      "total": 145435
    },
    "nino": "AA000000A",
    "taxCodeDetails": {
      "allowances": [
        {
          "amount": 2260,
          "componentType": 0,
          "description": "Tax Free Amount"
        },
        {
          "amount": 63,
          "componentType": 6,
          "description": "Gift Aid payments"
        },
        {
          "amount": 50,
          "componentType": 5,
          "description": "Personal Pension Relief"
        }
      ],
      "deductions": [
        {
          "amount": 3750,
          "componentType": 42,
          "description": "Child Benefit"
        },
        {
          "amount": 367,
          "componentType": 35,
          "description": "Underpayment Amount"
        },
        {
          "amount": 13750,
          "componentType": 32,
          "description": "savings income taxable at higher rate"
        },
        {
          "amount": 100,
          "componentType": 1,
          "description": "state pension state benefits"
        },
        {
          "amount": 100,
          "componentType": 27,
          "description": "property income"
        },
        {
          "amount": 100,
          "componentType": 45,
          "description": "In Year Adjustment "
        },
        {
          "amount": 100,
          "componentType": 5,
          "description": "IB taxable incapacity benefit"
        },
        {
          "amount": 5000,
          "componentType": 23,
          "description": "interest without tax taken off (gross interest)"
        },
        {
          "amount": 100,
          "componentType": 38,
          "description": "Employment Support Allowance"
        },
        {
          "amount": 21816,
          "componentType": 40,
          "description": "Adjustment to Rate Band"
        },
        {
          "amount": 422,
          "componentType": 41,
          "description": "Outstanding Debt Restriction"
        },
        {
          "amount": 100,
          "componentType": 18,
          "description": "Jobseeker 's allowance"
        },
        {
          "amount": 14835,
          "componentType": 7
        }
      ],
      "employment": [
        {
          "id": 1,
          "name": "Test Employer",
          "taxCode": "K5817"
        },
        {
          "id": 2,
          "name": "Test Pension Provider",
          "taxCode": "D0"
        }
      ],
      "splitAllowances": false,
      "taxCode": [
        {
          "taxCode": "K"
        },
        {
          "rate": 40,
          "taxCode": "D0"
        }
      ],
      "taxCodeDescriptions": [
        {
          "name": "Test Employer",
          "taxCode": "K5817",
          "taxCodeDescriptors": [
            {
              "taxCode": "K"
            }
          ]
        },
        {
          "name": "Test Pension Provider",
          "taxCode": "D0",
          "taxCodeDescriptors": [
            {
              "rate": 40,
              "taxCode": "D0"
            }
          ]
        }
      ],
      "total": -58167
    },
    "totalLiability": {
      "childBenefitTaxDue": 1500,
      "liabilityAdditions": {
        "excessGiftAidTax": {
          "amountInTermsOfTax": 0,
          "codingAmount": 0
        },
        "excessWidowsAndOrphans": {
          "amountInTermsOfTax": 0,
          "codingAmount": 0
        },
        "pensionPaymentsAdjustment": {
          "amountInTermsOfTax": 0,
          "codingAmount": 0
        }
      },
      "liabilityReductions": {
        "concessionalRelief": {
          "amountInTermsOfTax": 0,
          "codingAmount": 0
        },
        "doubleTaxationRelief": {
          "amountInTermsOfTax": 0,
          "codingAmount": 0
        },
        "enterpriseInvestmentSchemeRelief": {
          "amountInTermsOfTax": 0,
          "codingAmount": 0
        },
        "maintenancePayments": {
          "amountInTermsOfTax": 0,
          "codingAmount": 0
        }
      },
      "nonCodedIncome": {
        "totalIncome": 100,
        "totalTax": 40,
        "totalTaxableIncome": 100
      },
      "outstandingDebt": 169,
      "taxCreditOnUKDividends": 2000,
      "taxOnBankBSInterest": 1000,
      "totalTax": 48244,
      "underpaymentPreviousYear": 147
    },
    "version": 1
  },
  "taxableIncome": {
    "benefitsData": [
      {
        "a": "Travel and subsistence for Test Employer",
        "b": "12,000",
        "c": "The amounts received for the cost of travelling on business.",
        "d": "",
        "e": 1,
        "f": 53
      },
      {
        "a": "Vouchers and credit cards for Test Employer",
        "b": "100",
        "c": "The value of vouchers and credit cards provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 54
      },
      {
        "a": "Travel and subsistence for Test Employer",
        "b": "100",
        "c": "The amounts received for the cost of travelling on business.",
        "d": "",
        "e": 1,
        "f": 53
      },
      {
        "a": "Qualifying relocation expenses for Test Employer",
        "b": "100",
        "c": "All relocation costs under \u00a38,000 paid for by your employer(s).",
        "d": "",
        "e": 1,
        "f": 50
      },
      {
        "a": "Personal incidental expenses for Test Employer",
        "b": "100",
        "c": "The value of other personal expenses.",
        "d": "",
        "e": 1,
        "f": 49
      },
      {
        "a": "Payments of employee\u2019s behalf for Test Employer",
        "b": "100",
        "c": "The amount your employer(s) has paid for items on your behalf.",
        "d": "",
        "e": 1,
        "f": 48
      },
      {
        "a": "Other items for Test Employer",
        "b": "100",
        "c": "Other non-specific benefits you\u2019ve received.",
        "d": "",
        "e": 1,
        "f": 47
      },
      {
        "a": "Nursery places for Test Employer",
        "b": "100",
        "c": "The value of childcare provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 46
      },
      {
        "a": "Non-qualifying relocation expenses for Test Employer",
        "b": "100",
        "c": "All relocation costs over \u00a38,000 paid for by your employer(s).",
        "d": "",
        "e": 1,
        "f": 45
      },
      {
        "a": "Mileage for Test Employer",
        "b": "100",
        "c": "The amount your employer(s) paid for mileage.",
        "d": "",
        "e": 1,
        "f": 44
      },
      {
        "a": "Income Tax paid but not deducted from director\u2019s remuneration for Test Employer",
        "b": "100",
        "c": "Income Tax you paid but that was not deducted from your director\u2019s salary.",
        "d": "",
        "e": 1,
        "f": 52
      },
      {
        "a": "Expenses for Test Employer",
        "b": "100",
        "c": "The amount of expenses paid for by your employer(s).",
        "d": "",
        "e": 1,
        "f": 43
      },
      {
        "a": "Entertaining for Test Employer",
        "b": "100",
        "c": "The value of entertaining business clients paid for by your employer(s).",
        "d": "",
        "e": 1,
        "f": 42
      },
      {
        "a": "Employer provided services for Test Employer",
        "b": "100",
        "c": "The value of any services paid for by your employer.",
        "d": "",
        "e": 1,
        "f": 8
      },
      {
        "a": "Employer provided professional subscriptions for Test Employer",
        "b": "100",
        "c": "Professional subscriptions paid for by your employer(s).",
        "d": "",
        "e": 1,
        "f": 51
      },
      {
        "a": "Education services for Test Employer",
        "b": "100",
        "c": "The value of external training provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 41
      },
      {
        "a": "Assets transferred for Test Employer",
        "b": "100",
        "c": "The value of items given to you by your employer at no cost or less than market value. For example; a previous company car.",
        "d": "",
        "e": 1,
        "f": 40
      },
      {
        "a": "Assets for Test Employer",
        "b": "100",
        "c": "The value of assets, such as PCs and TVs, provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 39
      },
      {
        "a": "Accommodation for Test Employer",
        "b": "100",
        "c": "The value of accommodation provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 38
      },
      {
        "a": "Non-Cash benefit for Test Employer",
        "b": "100",
        "c": "A taxable benefit provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 117
      },
      {
        "a": "Beneficial loan for Test Employer",
        "b": "100",
        "c": "The value of an interest free or low interest loan provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 37
      },
      {
        "a": "Van fuel benefit for Test Employer",
        "b": "100",
        "c": "The value of van fuel benefit provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 36
      },
      {
        "a": "Van benefit for Test Employer",
        "b": "100",
        "c": "The value of the benefit on the van(s) provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 35
      },
      {
        "a": "Taxable expenses benefits for Test Employer",
        "b": "100",
        "c": "This is a payment made by your employer for using your own car to carry out your employer\u2019s business.",
        "d": "",
        "e": 1,
        "f": 34
      },
      {
        "a": "Service benefits for Test Employer",
        "b": "100",
        "c": "The amount based on taxable long service awards that your employer(s) give(s) you.",
        "d": "",
        "e": 1,
        "f": 33
      },
      {
        "a": "Telephone for Test Employer",
        "b": "100",
        "c": "The amount you receive from your employer(s) for use of your telephone.",
        "d": "",
        "e": 1,
        "f": 32
      },
      {
        "a": "Car benefit for Test Employer",
        "b": "100",
        "c": "The value of the benefit on the car(s) provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 31
      },
      {
        "a": "Medical insurance for Test Employer",
        "b": "100",
        "c": "The value of medical benefit(s) provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 30
      },
      {
        "a": "Car fuel benefit for Test Employer",
        "b": "100",
        "c": "The value of car fuel benefit provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 29
      },
      {
        "a": "Vouchers and credit cards for Test Employer",
        "b": "35",
        "c": "The value of vouchers and credit cards provided by your employer(s).",
        "d": "",
        "e": 1,
        "f": 54
      }
    ],
    "benefitsTotal": 14835,
    "employmentPension": {
      "hasEmployment": true,
      "isOccupationalPension": true,
      "taxCodeIncomes": {
        "employments": {
          "taxCodeIncomes": [
            {
              "employmentId": 1,
              "employmentPayeRef": "BT456",
              "employmentStatus": 1,
              "employmentType": 1,
              "income": 50000,
              "incomeType": 0,
              "isEditable": true,
              "isLive": true,
              "isOccupationalPension": false,
              "isPrimary": true,
              "jobTitle": "Job Title 1",
              "name": "Test Employer",
              "startDate": "2010-01-01",
              "tax": {
                "actualTaxDueAssumingBasicRateAlreadyPaid": 26949.8,
                "allowReliefDeducts": -58267,
                "taxBands": [
                  {
                    "income": 31785,
                    "lowerBand": 0,
                    "rate": 20,
                    "tax": 6357,
                    "upperBand": 31785
                  },
                  {
                    "income": 76482,
                    "lowerBand": 31785,
                    "rate": 40,
                    "tax": 30592.8,
                    "upperBand": 150000
                  },
                  {
                    "income": 0,
                    "lowerBand": 150000,
                    "rate": 45,
                    "tax": 0
                  }
                ],
                "totalIncome": 50000,
                "totalTax": 36949.8,
                "totalTaxableIncome": 108267
              },
              "taxCode": "K5817",
              "worksNumber": "WN 1"
            }
          ],
          "totalIncome": 50000,
          "totalTax": 36949.8,
          "totalTaxableIncome": 108267
        },
        "hasDuplicateEmploymentNames": false,
        "occupationalPensions": {
          "taxCodeIncomes": [
            {
              "employmentId": 2,
              "employmentPayeRef": "AS456",
              "employmentStatus": 1,
              "employmentType": 2,
              "income": 50000,
              "incomeType": 1,
              "isEditable": true,
              "isLive": true,
              "isOccupationalPension": true,
              "isPrimary": false,
              "jobTitle": "Job Title 2",
              "name": "Test Pension Provider",
              "startDate": "2010-01-01",
              "tax": {
                "actualTaxDueAssumingBasicRateAlreadyPaid": 11273.36,
                "totalIncome": 50000,
                "totalTax": 11273.36,
                "totalTaxableIncome": 50000
              },
              "taxCode": "D0",
              "worksNumber": "WN 2"
            }
          ],
          "totalIncome": 50000,
          "totalTax": 11273.36,
          "totalTaxableIncome": 50000
        },
        "totalIncome": 100000,
        "totalTax": 48223.16,
        "totalTaxableIncome": 158267
      },
      "totalEmploymentPensionAmt": 100000
    },
    "hasChanges": false,
    "income": 145435,
    "incomeTax": 48244,
    "increasesTax": {
      "benefitsFromEmployment": {
        "amount": 14835,
        "componentType": 0,
        "description": "",
        "iabdSummaries": [
          {
            "amount": 12000,
            "description": "Travel and Subsistence",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 53
          },
          {
            "amount": 100,
            "description": "Vouchers and Credit Cards",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 54
          },
          {
            "amount": 100,
            "description": "Travel and Subsistence",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 53
          },
          {
            "amount": 100,
            "description": "Qualfying Relocation Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 50
          },
          {
            "amount": 100,
            "description": "Personal Incidental Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 49
          },
          {
            "amount": 100,
            "description": "Payments on Employee's Behalf",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 48
          },
          {
            "amount": 100,
            "description": "Other Items",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 47
          },
          {
            "amount": 100,
            "description": "Nursery Places",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 46
          },
          {
            "amount": 100,
            "description": "Non-qualifying Relocation Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 45
          },
          {
            "amount": 100,
            "description": "Mileage",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 44
          },
          {
            "amount": 100,
            "description": "Income Tax Paid but not deducted from Director's Remuneration",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 52
          },
          {
            "amount": 100,
            "description": "Expenses",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 43
          },
          {
            "amount": 100,
            "description": "Entertaining",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 42
          },
          {
            "amount": 100,
            "description": "Employer Provided Services",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 8
          },
          {
            "amount": 100,
            "description": "Employer Provided Professional Subscription",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 51
          },
          {
            "amount": 100,
            "description": "Educational Services",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 41
          },
          {
            "amount": 100,
            "description": "Asset Transfer",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 40
          },
          {
            "amount": 100,
            "description": "Assets",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 39
          },
          {
            "amount": 100,
            "description": "Accommodation",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 38
          },
          {
            "amount": 100,
            "description": "Non-Cash Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 117
          },
          {
            "amount": 100,
            "description": "Beneficial Loan",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 37
          },
          {
            "amount": 100,
            "description": "Van Fuel Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 36
          },
          {
            "amount": 100,
            "description": "Van Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 35
          },
          {
            "amount": 100,
            "description": "Taxable Expenses Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 34
          },
          {
            "amount": 100,
            "description": "Service Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 33
          },
          {
            "amount": 100,
            "description": "Telephone",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 32
          },
          {
            "amount": 100,
            "description": "Car Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 31
          },
          {
            "amount": 100,
            "description": "Medical Insurance",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 30
          },
          {
            "amount": 100,
            "description": "Car Fuel Benefit",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 29
          },
          {
            "amount": 35,
            "description": "Vouchers and Credit Cards",
            "employmentId": 1,
            "employmentName": "Test Employer",
            "iabdType": 54
          }
        ]
      },
      "incomes": {
        "noneTaxCodeIncomes": {
          "bankBsInterest": {
            "amount": 5000,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 5000,
                "description": "Bank / Building Society Interest",
                "employmentId": 0,
                "iabdType": 75
              }
            ]
          },
          "dividends": {
            "amount": 20000,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 20000,
                "description": "UK Dividend",
                "employmentId": 0,
                "iabdType": 76
              }
            ]
          },
          "otherIncome": {
            "amount": 200,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 100,
                "description": "Profit",
                "employmentId": 0,
                "iabdType": 72
              },
              {
                "amount": 100,
                "description": "Non-Coded Income",
                "employmentId": 0,
                "iabdType": 19
              }
            ]
          },
          "statePension": 100,
          "taxableStateBenefit": {
            "amount": 300,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 100,
                "description": "Employment and Support Allowance",
                "employmentId": 0,
                "iabdType": 123
              },
              {
                "amount": 100,
                "description": "Job Seekers Allowance",
                "employmentId": 0,
                "iabdType": 84
              },
              {
                "amount": 100,
                "description": "Incapacity Benefit",
                "employmentId": 0,
                "iabdType": 83
              }
            ]
          },
          "totalIncome": 30600,
          "untaxedInterest": {
            "amount": 5000,
            "componentType": 0,
            "description": "",
            "iabdSummaries": [
              {
                "amount": 5000,
                "description": "Untaxed Interest",
                "employmentId": 0,
                "iabdType": 82
              }
            ]
          }
        },
        "taxCodeIncomes": {
          "employments": {
            "taxCodeIncomes": [
              {
                "employmentId": 1,
                "employmentPayeRef": "BT456",
                "employmentStatus": 1,
                "employmentType": 1,
                "income": 50000,
                "incomeType": 0,
                "isEditable": true,
                "isLive": true,
                "isOccupationalPension": false,
                "isPrimary": true,
                "jobTitle": "Job Title 1",
                "name": "Test Employer",
                "startDate": "2010-01-01",
                "tax": {
                  "actualTaxDueAssumingBasicRateAlreadyPaid": 26949.8,
                  "allowReliefDeducts": -58267,
                  "taxBands": [
                    {
                      "income": 31785,
                      "lowerBand": 0,
                      "rate": 20,
                      "tax": 6357,
                      "upperBand": 31785
                    },
                    {
                      "income": 76482,
                      "lowerBand": 31785,
                      "rate": 40,
                      "tax": 30592.8,
                      "upperBand": 150000
                    },
                    {
                      "income": 0,
                      "lowerBand": 150000,
                      "rate": 45,
                      "tax": 0
                    }
                  ],
                  "totalIncome": 50000,
                  "totalTax": 36949.8,
                  "totalTaxableIncome": 108267
                },
                "taxCode": "K5817",
                "worksNumber": "WN 1"
              }
            ],
            "totalIncome": 50000,
            "totalTax": 36949.8,
            "totalTaxableIncome": 108267
          },
          "hasDuplicateEmploymentNames": false,
          "occupationalPensions": {
            "taxCodeIncomes": [
              {
                "employmentId": 2,
                "employmentPayeRef": "AS456",
                "employmentStatus": 1,
                "employmentType": 2,
                "income": 50000,
                "incomeType": 1,
                "isEditable": true,
                "isLive": true,
                "isOccupationalPension": true,
                "isPrimary": false,
                "jobTitle": "Job Title 2",
                "name": "Test Pension Provider",
                "startDate": "2010-01-01",
                "tax": {
                  "actualTaxDueAssumingBasicRateAlreadyPaid": 11273.36,
                  "totalIncome": 50000,
                  "totalTax": 11273.36,
                  "totalTaxableIncome": 50000
                },
                "taxCode": "D0",
                "worksNumber": "WN 2"
              }
            ],
            "totalIncome": 50000,
            "totalTax": 11273.36,
            "totalTaxableIncome": 50000
          },
          "totalIncome": 100000,
          "totalTax": 48223.16,
          "totalTaxableIncome": 158267
        },
        "total": 130600
      },
      "total": 145435
    },
    "investmentIncomeData": [
      {
        "a": "Dividends",
        "b": "20,000",
        "c": "From 6 April 2016 up to \u00a35,000 of this dividend income is tax free. On amounts over \u00a35,000 tax is payable at 7.5%, 32.5% or 38.1% depending on your tax band. Your tax code is adjusted to collect any additional tax due."
      },
      {
        "a": "Taxed interest on savings and investments",
        "b": "5,000",
        "c": "This is the interest on your savings and investment income that has been taxed at source by the bank, building society or financial institution that you invest with."
      },
      {
        "a": "Untaxed interest on savings and investments",
        "b": "5,000",
        "c": "From 6 April 2016 banks and building societies, and some other investments, have paid interest without deducting tax. The amount shown is an estimate of the untaxed interest we expect you to receive in this tax year."
      }
    ],
    "investmentIncomeTotal": 30000,
    "otherIncomeData": [
      {
        "a": "Profit",
        "b": "100",
        "c": "The profit you made on the sale of UK property or land."
      },
      {
        "a": "Other income",
        "b": "100",
        "c": "Income from other sources, such as self employment or renting out a room, that won\u2019t be collected through your tax code."
      }
    ],
    "otherIncomeTotal": 200,
    "taxCodeList": [
      "K5817",
      "D0"
    ],
    "taxFreeAmount": 2260,
    "taxableBenefitsData": [
      {
        "a": "Employment and Support Allowance",
        "b": "100",
        "c": "The amount of Employment and Support Allowance you received."
      },
      {
        "a": "Jobseeker\u2019s Allowance",
        "b": "100",
        "c": "The amount of Jobseeker\u2019s Allowance you received."
      },
      {
        "a": "Incapacity Benefit",
        "b": "100",
        "c": "The amount of Incapacity Benefit you received."
      },
      {
        "a": "State Pension or other state benefits",
        "b": "100",
        "c": "The amount of State Pension or other State Benefits you receive."
      }
    ],
    "taxableBenefitsTotal": 400
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



