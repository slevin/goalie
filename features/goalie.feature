Feature: Adding new commitments
  Here are some commitments

  Scenario: Add one commitment
    When I press "M-x"
    And I type "goalie"
    Then I should be in "Goalie" mode
