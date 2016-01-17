# latte-compiler

A Clojure compiler of [Latte programming language](http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2015/Latte/)

[![Circle CI](https://circleci.com/gh/cezarykosko/latte-compiler/tree/master.svg?style=svg&circle-token=cbc1438cf282e14f6f205871ac71223e36bc00f5)](https://circleci.com/gh/cezarykosko/latte-compiler/tree/master)

## Usage

After calling `make` the `latc_x86`, `latc` and `latc_drip` binaries are ready to use. Calling `latc_x86 foo/bar/baz.lat` (or `latc foo/bar/baz.lat`, or `latc_drip foo/bar/baz.lat`) will, should code compile, create files `baz.s` (x86 assembly code) and `baz` (the executable) in the `foo/bar` directory.

## 3rd party libraries

Project utilizes Leiningen (script provided in lib/) to manage dependencies (requires Internet connection!)
Also, since Clojure's really slowing down JVM start time (all core namespaces need to be loaded, which slows down the compilator), drip is provided to be used (optionally, through `latc_drip` or setting `LATC_DRIP_ENABLED` envvar to `1`).
The dependencies include
- basic [Clojure](http://clojure.org) package with [core.match](https://github.com/clojure/core.match) for pattern matching and [algo.monads](https://github.com/clojure/algo.monads) for monads support
- [instaparse](https://github.com/engelberg/instaparse) for ABNF grammar parsing

## catalogue structure

- resources - Clojure resources dir, contains the grammar
- src - sources (`src/latte_compiler` - Clojure sources, `src/utils.c` - helper functions library)
- lib - directory containing leiningen & drip script
- project.clj - leiningen config

## Slow startup issue

Since Clojure core namespaces load on every execution, the JVM startup is really sluggish. Hence the `latc_drip` and `latc_drip_cleanup` binaries. They utilize [![drip](https://github.com/ninjudd/drip)], a small lib keeping the JVM running for another run, so that every second compilation does not have to load all namespaces (the JVM instance is killed after 30 minutes if not used). Note that every recompilation, kill and such may corrupt the classpath loader, and upon such the `latc_drip_cleanup` script should be called to fix that.
`latc_drip` is to be used instead of `latc` or `latc` should be called with `LATC_DRIP_ENABLED` var set to `1`.
Note that while that approach is generally faster (up to 2x) for a number of consecutive runs, it may cause problems should the classpath be corrupted and as such, it may be worth to re-run compilation after cleanup or on regular JVM if compilation stalls/weird errors are thrown.
