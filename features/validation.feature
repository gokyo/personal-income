Feature: Example Service Validation

  Background:
    Given header 'Accept' is 'valid'

  Scenario: Get BAD_REQUEST response for wrong UTR
    When I GET the SANDBOX resource '/sandbox/sa/inv@lid123K/example'
    Then the status code should be 'BAD_REQUEST'
    And I should receive JSON response:
  """
    {
        "code": "SA_UTR_INVALID",
        "message": "The provided SA UTR is invalid"
    }
    """