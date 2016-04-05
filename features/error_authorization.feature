Feature: Individual's Employment Tax Bearer token validation


  Background:
    Given header 'Accept' is 'valid'

  Scenario: Get UNAUTHORIZED response for Individual Tax Annual Summary due to wrong Bearer token
    Given I am not authorized due to 'wrong' Bearer token with utr '2234567890K'
    And I GET the LIVE resource '/sa/2234567890K/example'
    Then the status code should be 'UNAUTHORIZED'
    And I should receive JSON response:
    """
    {
        "code": "UNAUTHORIZED",
        "message": "Bearer token is missing or not authorized"
    }
    """

  Scenario: Get UNAUTHORIZED response for Individual Tax Annual Summary due to missing Bearer token
    Given I am not authorized due to 'missing' Bearer token with utr '2234567890K'
    And I GET the LIVE resource '/sa/2234567890K/example'
    Then the status code should be 'UNAUTHORIZED'
    And I should receive JSON response:
    """
    {
        "code": "UNAUTHORIZED",
        "message": "Bearer token is missing or not authorized"
    }
    """


