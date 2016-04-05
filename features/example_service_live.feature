Feature: Fetch Example for Live

  Background:
    Given header 'Accept' is 'valid'
    Given DES response to url '/des-example-service/sa/2234567890K/example' is:
    """
    {
      "text": "example",
      "number": 2.00
    }
    """

  Scenario: Fetch Example for Live
    When I am authorized with a valid Bearer token with utr '2234567890K'
    And I GET the LIVE resource '/sa/2234567890K/example'
    Then the status code should be 'OK'
    And I should receive JSON response:
    """
    {
      "text": "example",
      "number": 2.00
    }
    """