Feature: Fetch Example for Sandbox

  Background:
    Given header 'Accept' is 'valid'

  Scenario: Fetch Example for Sandbox
    When I GET the SANDBOX resource '/sandbox/sa/2234567890K/example'
    Then the status code should be 'OK'
    And I should receive JSON response:
    """
    {
      "text": "example",
      "number": 1.00
    }
    """
