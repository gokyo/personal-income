personal-income
=============================================

[![Build Status](https://travis-ci.org/hmrc/personal-income.svg?branch=master)](https://travis-ci.org/hmrc/personal-income) [ ![Download](https://api.bintray.com/packages/hmrc/releases/personal-income/images/download.svg) ](https://bintray.com/hmrc/releases/personal-income/_latestVersion)

Allows users to view their paye tax information and perform a renewal.

Requirements
------------

The following services are exposed from the micro-service.

Please note it is mandatory to supply an Accept HTTP header to all below services with the value ```application/vnd.hmrc.1.0+json```. 

API
---

| *Task* | *Supported Methods* | *Description* |
|--------|----|----|
| ```/income/:nino/tax-summary/:taxYear``` | GET | Returns the ```Tax Summary``` for the given nino. [More...](docs/tax-summary.md)  |
| ```/income/:nino/tax-credits/:renewalReference/auth``` | GET | Validate and retrieve the TCR auth-token assoicated with the NINO and renewal reference. [More...](docs/authenticate.md)|
| ```/income/:nino/tax-credits/claimant-details``` | GET | Retrieve the claiment-details associated with the nino. Note the header tcrAuthToken must be supplied. [More...](docs/claimentDetails.md) |
| ```/income/:nino/tax-credits/renewal``` | POST | Post a renewal to the NTC micro-service for off-line processing. Note the header tcrAuthToken must be supplied. [More...](docs/renewal.md)|
| ```/income/:nino/tax-credits/tax-credits-summary``` | GET | Fetch the Tax Credits Summary object for a given NINO. [More...](docs/tax-credits-summary.md)|
| ```/income/tax-credits/submission/state/enabled``` | GET | This endpoint retrieves the current state of tax credit submissions. [More...](docs/tax-credits-submission-state-enabled.md)|

# Sandbox
All the above endpoints are accessible on sandbox with `/sandbox` prefix on each endpoint,e.g.
```
    GET /sandbox/income/:nino/tax-summary/:taxYear
```

# Definition
API definition for the service will be available under `/api/definition` endpoint.
See definition in `/conf/api-definition.json` for the format.

# Version
Version of API need to be provided in `Accept` request header
```
Accept: application/vnd.hmrc.v1.0+json
```
