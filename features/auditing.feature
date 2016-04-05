Feature: Auditing Individual Tax Annual Summary for Live

  Background:
    Given The auditing service is up
    Given header 'Accept' is 'valid'
    Given I am authorized with a valid Bearer token with utr '2234567890K'
    Given DES response to url '/des-example-service/sa/2234567890K/example' is:
    """
    {
      "text": "example",
      "number": 2.00
    }
    """


  Scenario: Get Live Individual Tax Annual Summary is audited
    And I GET the LIVE resource '/sa/2234567890K/example'
    Then a simple event with source 'api-microservice-template' and type 'ServiceResponseSent' has been audited with:
      | tags   | transactionName   | fetchExample  |
      | detail | saUtr             | 2234567890K         |

  Scenario: Get Live Individual Tax Annual Summary is OK despite audit error
    Given The auditing service is in error
    And I GET the LIVE resource '/sa/2234567890K/example'
    Then the status code should be 'OK'

  Scenario: Get Sandbox Individual Tax Annual Summary is NOT audited
    When I GET the SANDBOX resource '/sandbox/sa/2234567890K/example'
    Then No event has been audited

  Scenario: Get Live Individual Tax Annual Summary is audited
    Given DES response to url '/des-example-service/sa/2234567890K/example' is error 'INTERNAL_SERVER_ERROR'
    When I GET the LIVE resource '/sa/2234567890K/example'
    Then the status code should be 'INTERNAL_SERVER_ERROR'
    And No event has been audited