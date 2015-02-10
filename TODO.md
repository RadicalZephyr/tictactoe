## Little
- Test AI performance in tricky sub-cases
- Improve test coverage in general
- Remove dev conveniences in tictactoe.gui

## Big
- Fix AI so it can't be beaten
- Change GUI structure
  I want to make it so that the player can decide who goes
  first. Current structure makes that DIFFICULT. But, Mirami's
  suggestion is to change the whole interface to two buttons, and a
  message indicating win or loss in the last game. This structure is
  nice because it means we can maybe use a closure (message-passing
  type object?) to actually encapsulate all of the state. Or at least
  the portion that deals with who has what mark and who plays first...
