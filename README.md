
Parsing Large Files
====================

This repository contains a couple executables showing different ways to parse
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

`large-file-dumb-parser` is a standard parsec parser.  It returns a `FullFile`
type (which is defined in [Types.hs](src/Types.hs)).  The source is in
[DumbFileParser.hs](app/DumbFileParser.hs).

It is "dumb" because it reads all of the input into memory, then turns it into
a data structure in memory.

You can run it with the following command:

```sh
$ time stack exec -- large-file-dumb-parser
Killed

real    1m10.517s
user    0m43.810s
sys     0m44.553s
```

The system runs out of memory after a minute and `large-file-dumb-parser` is
killed.

## Streaming Parser

`large-file-streaming-parser` is based on Conduit.  It streams lines from the
file and handles them one-at-a-time.  The source is in
[StreamingFileParser.hs](app/StreamingFileParser.hs).

```sh
$ time stack exec -- large-file-streaming-parser
Succeed

real    0m6.769s
user    0m4.370s
sys     0m2.473s
```

Using the `RTS` options of the Haskell runtime,  we can tell the Haskell
runtime to make sure it doesn't use more than a megabyte of heap space.  With
this option, we know that `large-file-streaming-parser` is able to run in
constant heap space.  This makes sense, since it is only operating on the file
one line at at time:

```sh
$ stack exec -- large-file-streaming-parser +RTS -M1M
Succeed
```

The big downside of `large-file-streaming-parser` is that it is somewhat more
hacky than a Parsec parser.  Operating on a file line-by-line is similar to how
a parser would be written in an imperative lanuage.  Ideally, we would have a
Parsec-like parser that runs in constant memory.

## Smart Parser

`large-file-smart-parser` is a Parsec parser that runs in constant memory using
lazy IO.  The source is in [SmartFileParser.hs](app/SmartFileParser.hs).

```sh
$ time stack exec -- large-file-smart-parser
Successfully parsed.

real    0m51.958s
user    0m41.947s
sys     0m20.097s
```

Make sure it runs in constant memory:

```
$ stack exec -- large-file-smart-parser +RTS -M1M
Successfully parsed.
```

`large-file-smart-parser` also runs in constant memory.  It is much easier to
understand than the streaming parser.  It relies on lazy IO to read in the file
as a `String`.  It does not accumulate any memory while parsing the file.

### manyLength

As an aside, I actually had a lot of trouble writing the [`manyLength`]()
function in [SmartFileParser.hs](app/SmartFileParser.hs).

I asked two questions on Stack Overflow about it:

- https://stackoverflow.com/questions/43092461/heap-usage-for-cps-vs-non-cps-parsers-in-haskells-parsec
- https://stackoverflow.com/questions/43119278/how-to-tell-whether-parsec-parser-uses-constant-heap-space-in-haskell

## Hybrid Approach

The best solution might be some sort of hybrid approach of the streaming Parser
and smart parser.  You might be able to write a Parsec-like parser that gets
input from a conduit `Producer` and sends data to a conduit `Consumer`.

This might give you both speed and simplicity.
