Feature: Error cases for Individual Tax on Live

  Background:
    Given I am authorized with a valid Bearer token with utr '2234567890K'
    And header 'Accept' is 'valid'

  Scenario Outline: Fetch Example and DES returns error
    Given DES response to url '/des-example-service/sa/2234567890K/example' is error '<DES_ERROR_CODE>'
    And I GET the LIVE resource '/sa/2234567890K/example'
    Then the status code should be 'INTERNAL_SERVER_ERROR'
    And I should receive JSON response:
     """
    {
        "code": "INTERNAL_SERVER_ERROR",
        "message": "Internal server error"
    }
    """
    Examples:
      | DES_ERROR_CODE        |
      | INTERNAL_SERVER_ERROR |
      | BAD_GATEWAY           |
      | CONFLICT              |
      | REQUEST_TIMEOUT       |


  Scenario: Fetch Example and DES returns 404 error
    Given DES response to url '/des-example-service/sa/2234567890K/example' is error 'NOT_FOUND'
    When I GET the LIVE resource '/sa/2234567890K/example'
    Then the status code should be 'NOT_FOUND'
    And I should receive JSON response:
     """
    {
        "code": "NOT_FOUND",
        "message": "Resource was not found"
    }
    """