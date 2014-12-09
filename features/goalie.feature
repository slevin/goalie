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

  Scenario: Move previous with two hilights first
    Given I start Goalie
    And I add commitment "commit1"
    And I add commitment "commit2"
    When I press "p"
    And I press "p"
    Then "commit1" should be hilighted
    And "commit2" should not be hilighted

  Scenario: Move previous/next
    Given I start Goalie
    And I add commitment "commit1"
    When I press "p"
    And I press "n"
    Then "Add Commitment" should be hilighted

  Scenario: Move previous/previous/next
    Given I start Goalie
    And I add commitment "commit1"
    And I add commitment "commit2"
    When I press "p"
    And I press "p"
    And I press "n"
    Then "commit2" should be hilighted

Feature: Saving
  Scenario: Add commitment, quit, start
    Given I start Goalie
    And I add commitment "commit1"
    When I quit Goalie
    And I start Goalie
    Then "commit1" should be a commitment

Feature: Deleting
  Scenario: Add, delete, confirm
    Given I start Goalie
    And I add commitment "commit1"
    And I press "p"
    When I press "d"
    And I start an action chain
    And I type "yes"
    And I execute the action chain
    Then "commit1" should not be a commitment
    And "Add Commitment" should be hilighted

    




