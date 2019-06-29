# Minesweeper

Basic `ncurses` minesweeper game!

## Quickstart

To build:

```sh
make
```

Then, to start a game with 5 bombs (the size of the grid is currently fixed to
10 by 20):

```sh
./bin/main 5
```

## Controls

+ Arrows / Vim controls to move the cursor around.
+ `d` to reveal the position under the cursor.
+ `f` to flag the position under the cursor as containing a bomb.
+ `q` to quit.

The game ends either when a bomb is revealed (loss), or all safe positions have
been revealed (win).
