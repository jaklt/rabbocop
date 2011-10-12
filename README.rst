Rabbocop
========

Rabbocop is Arimaa bot. For more information about Arimaa see http://arimaa.com

The goal of this project is to compare AlphaBeta search with Monte Carlo Tree
search. Disclaimer: I am not supposed to develop strong bots with highly
tuned up static evaluation function, I use one not highly optimized.


Wanna play?
-----------

Requirements (i am sorry but I do not know all minimal needed versions)::

    python with WxPython
    bzr
    git
    ghc (>= 7.0.0)
    gcc
    make

Just type::

    git clone git@github.com:JackeLee/rabbocop.git
    cd rabbocop

and then ``make playMCTS`` or ``make playAB``. The playMCTS option is for
playing against bot using Monte Carlo Tree Search and the playAB option against
bot using AlphaBeta search.

Compile options:
----------------

Before compiling with additional options it is necessary to run ``make
clean``. You can change compilation options by adding some of the listed
options to ``make``:

    VERBOSE=n
        For IterativeAB program, this will print actual best move and score
        anytime a depth is fully explored.
        For MCTS it will print actual best move and score after each n
        iterations of UCT algorithm.

    EVAL=fairy
        Builds program using Fairy's evaluation function.

    NULL_MOVE=1
        Enables Null moves for AlphaBeta search.

    canPass=1
        Adds possibility to generate less than 4 moves.

    CORES=X
        Compiles program with the number of available CPU's set to X. (Default
        is 1) When running program compiled with CORES=X by yourself it is
        necessary to add +RTS -NX as additional arguments.

    WINDOW=X
        It switches on aspiration window option for AlphaBeta algorithm with
        Window set to X.

    noHH=1
        Disables history heuristics optimisations for MCTS.

    abHH=1
        Enables history heuristics optimisations for AlphaBeta.

    noHeavyPlayout=1
        This disables heuristic in playouts.

    PROF=1
        Enables profiling for bots.

    playMatch
        Starts new game AlphaBeta search versus MCTS. Use it instead of
        playMCTS or playAB.
