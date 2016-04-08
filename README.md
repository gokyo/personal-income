personal-income
=============================================

Allows users to view their paye tax information.

Requirements
------------

The following services are exposed from the micro-service.

API
---

| *Task* | *Supported Methods* | *Description* |
|--------|----|----|
| ```/income/:nino/tax-summary/:taxYear``` | GET | Returns a the ```Tax Summary``` for the given nino. [More...](docs/tax-summary.md)  |


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