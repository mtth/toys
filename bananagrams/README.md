# Bananagrams

To run the command using the default dictionary (`/usr/share/dict/words`):

```sh
$ stack run -- --hand=abcdef
 b
decaf
```

Or, specifying a custom dictionary (one word per line):

```sh
$ stack run -- --hand=abcdef --words_file=my/words
```

Haddock documentation is available
[here](http://mtth.github.io/toys/bananagrams/).
