# Tic-tac-toe Game Analysis

There is only one fundamental thing in tic-tac-toe and that is a
three-in-a-row.  We will call this an "attack".

Now, there are four different types of attacks.  There are potential
attacks, threats, shots, and void's. These distinctions are simply
descriptive of which player has claimed that attack.

A potential attack is one where no player has claimed any of the
squares in it.

A null attack is one where no player can claim because both players
have played in the attack at least once.

A threat is one in which the opposing player has claimed that attack
by placing at least one piece in it, and the other's are unclaimed. A
shot is the opposite, where the ai has claimed that attack. Both
threats and shots can have a priority, which is whether we have
claimed two of the slots in the attack or only one.
