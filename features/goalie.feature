Feature: Entering Goalie
  Scenario: I enter goalie mode
    Given I start Goalie
    Then "goalie-mode" should be active
    And "Add Commitment" should be hilighted

Feature: Adding Commitments
  Scenario: I add a commitment
    Given I start Goalie
    When I add commitment "commit1"
    Then "commit1" should be a commitment
    And "commit1" should not be hilighted

  Scenario: I add multiple commitments
    Given I start Goalie
    When I add commitment "new-commit1"
    And I add commitment "new-commit2"
    Then "new-commit1" should be a commitment
    And "new-commit2" should be a commitment

Feature: Moving
  Scenario: Move previous highlights previous
    Given I start Goalie
    And I add commitment "commit1"
    When I press "p"
    Then "commit1" should be hilighted
    And "Add Commitment" should not be hilighted



