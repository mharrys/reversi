Reversi
=======
Implementation of the classic board game Reversi (also known as Othello) in Haskell.

How-to
------
Follow instructions on how to install
[Stack](https://www.haskell.org/downloads).

To build the library, execute from root directory:

    $ stack build

To build and install the command-line interface, execute from root directory:

    $ stack install
    $ reversi

The `stack install` command is exactly the same as `stack build` with the only
difference that stack moves the binary to `~/.local/bin`. So in order to
execute the command-line interface you must have this directory in your `PATH`.
