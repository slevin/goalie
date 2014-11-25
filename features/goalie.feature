Feature: Adding new commitments
  Here are some commitments

  Scenario: Add one commitment
    Given I press "M-x"
    And I type "goalie"
    Then I should be in "Goalie" mode
