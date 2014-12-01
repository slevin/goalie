Feature: Entering Goalie
  Scenario: I enter goalie mode 1
    Given I start an action chain
    And I press "M-x"
    When I type "goalie"
    And I execute the action chain
    Then "goalie-mode" should be active

  Scenario: I enter goalie mode 2
    Given I start Goalie
    Then "goalie-mode" should be active

Feature: Adding Commitments
  Scenario: I add a commitment
    Given I start Goalie
    When I start an action chain
    And I press "RET"
    And I type "commit1"
    And I execute the action chain
    Then "commit1" should be a commitment
    And "commit1" should not be highlighted

  Scenario: I add multiple commitments
    Given I start Goalie
    When I add commitment "new-commit1"
    And I add commitment "new-commit2"
    Then "new-commit1" should be a commitment
    And "new-commit2" should be a commitment

Feature: Moving
  Scenario: Add starts highlighted
    Given I start Goalie
    Then "Add Commitment" should be highlighted

  Scenario: Move previous highlights previous
    Given I start Goalie
    And I add commitment "commit1"
    When I press "p"
    Then "commit1" should be highlighted
    And "Add Commitment" should not be highlighted



