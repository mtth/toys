# Tetris

A minimal but functional Tetris game featuring UIs written with
[Brick](https://hackage.haskell.org/package/brick) and
[Gloss](https://hackage.haskell.org/package/gloss).

<p align="center">
  <img src="https://i.imgur.com/ZqLo1Mz.png">
</p>

To play:

```sh
$ stack exec tetris-brick # Terminal UI.
$ stack exec tetris-gloss # Graphical window UI.
```

The following commands are supported:

+ `j` to move the active block down,
+ `h` to move it left,
+ `l` to move it right,
+ `f` to rotate it (clockwise),
+ `<esc>` to quit.

The underlying library's documentation is available
[here](https://mtth.github.io/toys/tetris).
