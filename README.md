
```sh
$ stack build
```

```sh
$ stack exec -- large-file-writer
```

`big-example-file`

```sh
$ time stack exec -- large-file-dumb-parser
Successfully parsed.

real    0m7.306s
user    0m6.063s
sys     0m3.507s
```

```sh
$ stack exec -- large-file-dumb-parser +RTS -M10M
large-file-dumb-parser: Heap exhausted;
large-file-dumb-parser: Current maximum heap size is 10485760 bytes (10 MB).
large-file-dumb-parser: Use `+RTS -M<size>' to increase it.
```

```sh
$ time stack exec -- large-file-streaming-parser
Succeed

real    0m1.721s
user    0m1.433s
sys     0m0.687s
```
