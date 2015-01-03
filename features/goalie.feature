Feature: Entering Goalie
  Scenario: I enter goalie mode
    Given I start Goalie
    Then "goalie-mode" should be active

Feature: Adding Commitments
  Scenario: I add a commitment
    Given I start Goalie
    When I add commitment "commit1"
    Then "commit1" should be a commitment
    And "commit1" should be hilighted

  Scenario: I add multiple commitments
    Given I start Goalie
    When I add commitment "new-commit1"
    And I add commitment "new-commit2"
    Then "new-commit1" should be a commitment
    And "new-commit2" should be a commitment

Feature: Moving
  Scenario: Move previous at beginning stays
    Given I start Goalie
    And I add commitment "commit1"
    When I move previous
    Then "commit1" should be hilighted

  Scenario: Move next at end stays
    Given I start Goalie
    And I add commitment "commit1"
    When I move next
    Then "commit1" should be hilighted

  Scenario: Move next
    Given I start Goalie
    And I add commitment "commit1"
    And I add commitment "commit2"
    When I move next
    Then "commit2" should be hilighted

  Scenario: Move next/previous
    Given I start Goalie
    And I add commitment "commit1"
    And I add commitment "commit2"
    When I move next
    And I move previous
    Then "commit1" should be hilighted

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
    And I move previous
    When I delete current commitment
    Then "commit1" should not be a commitment

  Scenario: Add, delete, restart
    Given I start Goalie
    And I add commitment "commit1"
    And I move previous
    When I delete current commitment
    And I quit Goalie
    and I start Goalie
    Then "commit1" should not be a commitment

Feature: Completeting
  Scenario: Add, complete
    Given I start Goalie
    And I add commitment "commit1"
    And I mark current as complete
    Then "commit1" should be a completed commitment

  Scenario: Add, skip
    Given I start Goalie
    And I add commitment "commit1"
    When I mark current as skip
    Then "commit1" should be a skipped commitment

  Scenario: Change
    Given I start Goalie
    And I add commitment "commit1"
    And I mark current as complete
    When I mark current as skip
    Then "commit1" should be a skipped commitment
