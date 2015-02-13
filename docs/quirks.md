# Quirks of the AI

Using the Minimax algorithm is effective, but it has one very
interesting quirk when combined with symmetry analysis and equivalent
move randomization. It essentially always prefers to finish creating a
fork, instead of immediately winning. It's very strange, and I'm not
sure I totally understand why it *prefers* to do that, but it makes
sense that it values both an immediate win, and an eventual assured
win (i.e. a successful fork) equally.
