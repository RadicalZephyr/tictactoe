# Quirks of the AI

Using the Minimax algorithm is effective, but it has one very
interesting quirk when combined with symmetry analysis and equivalent
move randomization. It essentially always prefers to finish creating a
fork, instead of immediately winning. It's very strange, and I'm not
sure I totally understand why it *prefers* to do that, but it makes
sense that it values both an immediate win, and an eventual assured
win (i.e. a successful fork) equally.


Update 2/15/2015: It turns out that this behavior goes deeper than
just preferring to finish a fork. If the AI achieves a fork and you
make a move that doesn't block one of the fork paths, it will continue
making essentially arbitrary moves. I think this is because it sees
the fork and then calculates that all moves result in it winning,
unless it fails to take the win after the fork is forced.

One possible solution to this perplexing/insulting behavior (it's like
being toyed with!) would be to put some weighting on the depth of the
win. Use 10 and -10 for wins, and then subtract the depth of the
recursion. This would make paths that lead to immediate wins more
attractive.

On the other hand, it still doesn't lose, and it's sort of an
intriguing behavior... Not sure if I want to get rid of it.
