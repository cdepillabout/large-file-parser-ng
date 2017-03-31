
Parsing Large Files
====================

This repository contains a couple executables that show different ways to parse
large files.

The two basic techniques are to use lazy IO (which is shown in the "Smart
Parser" below) and to use streaming IO (which is shown in the "Streaming
Parser" below).

## Building All Executables

If you have `stack` installed, it is easy to build all the executables.

```sh
$ stack setup   # install GHC if required
$ stack build   # build the executables
```

## Example File Creator

Running [FileWriter.hs](app/FileWriter.hs) will create a file called
`big-example-file` in the current directory.

```sh
$ time stack exec -- large-file-writer

real    0m19.020s
user    0m8.510s
sys     0m11.360s
```

This `big-example-file` is 10,000,000 lines and around 150 MB.  It looks like
this:

```
00 header
01 data 1
01 data 2
01 data 3
...
01 data 9999998
01 data 9999999
01 data 10000000
99 final 10000000
```

## Dumb File Parser

```sh
$ time stack exec -- large-file-dumb-parser
Killed

real    1m10.517s
user    0m43.810s
sys     0m44.553s
i
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

```sh
$ stack exec -- large-file-streaming-parser +RTS -M1M
Succeed
```


```sh
$ time stack exec -- large-file-smart-parser
Successfully parsed.

real    0m6.491s
user    0m5.880s
sys     0m1.713s
```


```
$ stack exec -- large-file-smart-parser +RTS -M1M
Successfully parsed.
```
