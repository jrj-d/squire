# Squire

Squire is an incomplete chess engine written in Scala.

Since my intent was to familiarize myself with the language, this project has several flaws:
as a Scala piece of code, it tries hard to follow Scala best practices and make use of some cool features of the language,
but it still represents my first steps in Scala and is bound to be immature; as a chess engine, it is not optimized very
far in terms of features and performance, as it is more of a learning project.
In particular, I had blinders on when I read about the immutable state paradigm in Scala, and implemented a chess
position as an immutable object. Now looking back, it would have made much more sense to use a mutable object for performance reasons.

## Engine description

Squire consists of two parts: chess implementation and search logic.
An interface called `GameState` allows the search logic to deal with any kind of game as long as a proper subclass of
`GameState` implementing that game is defined.

The chess implementation is rather awkward and still a lot buggy, but I'm working on writing a proper test suite.

The search algorithm is minimax (negamax) with alpha-beta pruning and moves ordering to increase the number of cut-offs.
The engine uses interative deepening. The evaluation function is a linear combination of features provided by the
chess implementation: number of pieces, threats and defense on important pieces.
I plan to adjust the weights of the linear combination using linear regression on the end of games, if I find time.

## Usage

There are two ways of interacting with the engine. The first, quick and dirty way is to use the class `Game` and play from within the console.
The better way is to interface Squire with XBoard. The file `XBoard.scala` provides a barebone client, that you can
call from XBoard by loading a new engine, with Squire's root folder as directory, and `sbt run ` as command.

## Todo

+ Start coding a test suite, which should have been done before...
+ Correct castling move for check restrictions (for now only checks if the king is in check at the end of the move)
+ Adjust evaluation function weights, or learn them
+ Implement quiescence search

[![Coverage Status](https://coveralls.io/repos/jrj-d/squire/badge.svg)](https://coveralls.io/r/jrj-d/squire)
