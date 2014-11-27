Feature: Adding new commitments
  Here are some commitments

  Scenario: Add one commitment
    Given I start an action chain
    And I press "M-x"
    When I type "goalie"
    And I execute the action chain
    Then "goalie-mode" should be active
