# latte-compiler

A Clojure compiler of [Latte programming language](http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2015/Latte/)

[![Circle CI](https://circleci.com/gh/cezarykosko/latte-compiler/tree/master.svg?style=svg&circle-token=cbc1438cf282e14f6f205871ac71223e36bc00f5)](https://circleci.com/gh/cezarykosko/latte-compiler/tree/master)

## Usage

After calling `make` the `latc_x86` and `latc` binaries are ready to use. Calling `latc_x86 foo/bar/baz.lat` (or `latc foo/bar/baz.lat`) will, should code compile, create files `baz.s` (x86 assembly code) and `baz` (the executable) in the `foo/bar` directory.

## 3rd party libraries

Project utilizes Leiningen (script provided in lib/) to manage dependencies (requires Internet connection!
The dependencies include
- basic [Clojure](http://clojure.org) package with [core.match](https://github.com/clojure/core.match) for pattern matching and [algo.monads](https://github.com/clojure/algo.monads) for monads support
- [instaparse](https://github.com/engelberg/instaparse) for ABNF grammar parsing

## catalogue structure

- resources - Clojure resources dir, contains the grammar
- src - sources (`src/latte_compiler` - Clojure sources, `src/utils.c` - helper functions library)
- lib - directory containing leiningen script
- project.clj - leiningen config


