# Squire

Squire is an incomplete chess engine written in Scala.

Since my intent was to familiarize myself with the search techniques in board games, the engine uses as few chess-specific optimizations as possible.
In particular, the move generation is a vanilla implementation that does not use bitboards or other efficient representations.


[![Build Status](https://travis-ci.org/jrj-d/squire.svg?branch=master)](https://travis-ci.org/jrj-d/squire) [![codecov](https://codecov.io/gh/jrj-d/squire/branch/master/graph/badge.svg)](https://codecov.io/gh/jrj-d/squire)

## Engine description

Squire consists of two parts: chess implementation and search logic.
An interface called `State` allows the search logic to deal with any kind of game as long as a proper subclass of
`State` implementing that game is defined.

The chess implementation is slow, because optimizing a chess engine was not the purpose of the project.
At least it's tested to some extent using perft.

The search algorithm is minimax (negamax) with alpha-beta pruning and moves ordering to increase the number of cut-offs.
The engine uses iterative deepening. 
The evaluation function comes from a [post](https://chessprogramming.wikispaces.com/Simplified+evaluation+function) by Tomasz Michniewski.

## Usage

Build Squire with `sbt assembly`.

Squire can interface with XBoard.
The class `squire.chess.connectors.xboard.Main` is a client to will print instructions to the standard input and read from
the standard output.

In XBoard, Squire can be loaded with a command line like
```
scala -classpath /path/to/squire/squire/jar/squire-assembly-0.1-SNAPSHOT.jar squire.chess.connectors.xboard.Main
```

## Todo

For chess:

+ Test search logic
+ Learn evaluation function
+ Implement quiescence search
+ Implement tests for all draw rules

Outside of chess:

+ Implement other games that require MCTS instead of exhaustive search
+ Learn evaluation functions using RL
