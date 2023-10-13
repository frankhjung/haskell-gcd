# Haskell Greatest Common Denominator

This is one of my early Haskell projects. I was using it to work out how to
structure a project and on what tools to use. Later, when Git pipelines became
available, I explored how to use these to build the project.

The code was not as important as the project structure. Professionally as an
Developer + SCM + DevOps + Consultant I deal with enterprise projects with the
following features:

* code is version controlled
* code is styled and formatted by tools
* building code is controlled by a build system (e.g. Maven, Gradle, Make, Stack, etc)
* there are unit tests
* there are performance benchmarks
* API documentation is included
* documentation (this [README](README.md), more often a wiki)

Originally I was using [Cabal](https://www.haskell.org/cabal/), but found that
[Stack](https://docs.haskellstack.org) managed dependencies and project phases
better. If you look at earlier commits you will see those attempts. Perhaps, you
can show me where I went wrong and should have stuck with Cabal?

In this project, I will show how to:

* build using [stack](https://docs.haskellstack.org)
* format cabal configuration files using [cabal-fmt](https://hackage.haskell.org/package/cabal-fmt)
* [lint](http://hackage.haskell.org/package/hlint) and [style](https://hackage.haskell.org/package/stylish-haskell) source code
* build using [GNU Make](https://www.gnu.org/software/make/) using [Stack](https://docs.haskellstack.org)
* unit test with [HSpec](http://hackage.haskell.org/package/hspec) and [QuickCheck](http://hackage.haskell.org/package/QuickCheck)
* benchmark with [Criterion](http://hackage.haskell.org/package/criterion)
* document using [Haddock](http://hackage.haskell.org/package/haddock)
* profile using [GHC](https://www.haskell.org/ghc/)

It uses a couple of Greatest Common Denominator (GCD) algorithms as examples for
testing and benchmarking. The GCD algorithms are described
[here](https://en.wikipedia.org/wiki/Euclidean_algorithm).

## Documentation

API documentation is available from:

* [GitHub Pages](https://frankhjung.github.io/haskell-gcd/)
  * [Benchmark](https://frankhjung.github.io/haskell-gcd/benchmark.html)
* [GitLab Pages](https://frankhjung1.gitlab.io/haskell-gcd)
  * [Benchmark](https://frankhjung1.gitlab.io/haskell-gcd/benchmark.html)

Documentation is produced using [Haddock](http://hackage.haskell.org/package/haddock).

## Setup

To prime environment with compiler, packages and dependencies, run the
environment setup (see [Makefile](Makefile) `setup` goal for full details):

```bash
make setup
```

Stack uses Cabal under the covers but does more package management.

## Build

Use the make goal `build`:

```bash
make build
```

For a rebuild of all targets run the `clean` goal first:

```bash
make clean build
```

## Tests

Run the `test` goal:

```bash
make test
```

### Example - test

```text
$ make test
gcd> test (suite: test)

euclid1
  euclid1 1 1
    returns 1
  euclid1 371 379904
    returns 371
euclid2
  euclid2 1 1
    returns 1
  euclid2 371 379904
    returns 371
gcd
  gcd 1 1
    returns 1
  gcd 371 379904
    returns 371

Finished in 0.0009 seconds
6 examples, 0 failures
```

## Benchmark

Benchmark the two Euclid _Greatest Common Denominator_ algorithms:

```bash
make bench
```

### Example - performance benchmark

```text
$ make bench
gcd> benchmarks
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking euclid1: /379904
time                 18.62 ns   (17.59 ns .. 19.73 ns)
                     0.983 R²   (0.973 R² .. 0.996 R²)
mean                 18.60 ns   (17.92 ns .. 19.83 ns)
std dev              3.045 ns   (1.540 ns .. 5.638 ns)
variance introduced by outliers: 97% (severely inflated)

benchmarking euclid2: /379904
time                 19.09 ns   (18.64 ns .. 19.60 ns)
                     0.995 R²   (0.992 R² .. 0.997 R²)
mean                 19.45 ns   (18.99 ns .. 20.04 ns)
std dev              1.810 ns   (1.376 ns .. 2.618 ns)
variance introduced by outliers: 91% (severely inflated)

benchmarking gcd: /379904
time                 18.51 ns   (17.86 ns .. 19.19 ns)
                     0.990 R²   (0.983 R² .. 0.995 R²)
mean                 18.88 ns   (18.28 ns .. 19.52 ns)
std dev              2.038 ns   (1.595 ns .. 2.628 ns)
variance introduced by outliers: 93% (severely inflated)

Benchmark benchmark: FINISH
Completed 2 action(s).
```

### Report

* [Benchmark Report](files/benchmark.html)

## Profiling

### Using GHC

The following shows how to profile this application using GHC:

* compile with profiling

  ```bash
  ghc -prof -fprof-auto -rtsopts app/Main.hs src/GCD.hs
  ```

* to profile run program with arguments:

```bash
app/Main 371 379904 +RTS -p
```

* results are reported in `Main.prof`:

```text
Fri Jul 27 23:05 2018 Time and Allocation Profiling Report  (Final)

   Main +RTS -p -RTS 371 379904

total time  =        0.00 secs   (1 ticks @ 1000 us, 1 processor)
total alloc =     112,632 bytes  (excludes profiling overheads)

COST CENTRE MODULE           SRC                         %time %alloc

euclid1     GCD              src/GCD.hs:(21,1)-(25,44)   100.0   43.6
CAF         GHC.IO.Handle.FD <entire-module>               0.0   30.8
CAF         GHC.IO.Encoding  <entire-module>               0.0    2.9
main        Main             app/Main.hs:(19,1)-(24,25)    0.0   12.0
main.(...)  Main             app/Main.hs:23:19-40          0.0    8.5


COST CENTRE          MODULE                SRC                        no.     entries  %time %alloc   %time %alloc

MAIN                 MAIN                  <built-in>                 118          0    0.0    0.6   100.0  100.0
 CAF                 Main                  <entire-module>            234          0    0.0    0.1     0.0    0.2
  main               Main                  app/Main.hs:(19,1)-(24,25) 236          1    0.0    0.0     0.0    0.0
 CAF                 GHC.Conc.Signal       <entire-module>            226          0    0.0    0.6     0.0    0.6
 CAF                 GHC.IO.Encoding       <entire-module>            214          0    0.0    2.9     0.0    2.9
 CAF                 GHC.IO.Encoding.Iconv <entire-module>            212          0    0.0    0.2     0.0    0.2
 CAF                 GHC.IO.Handle.FD      <entire-module>            204          0    0.0   30.8     0.0   30.8
 CAF                 GHC.IO.Handle.Text    <entire-module>            202          0    0.0    0.1     0.0    0.1
 CAF                 Text.Read.Lex         <entire-module>            170          0    0.0    0.6     0.0    0.6
 main                Main                  app/Main.hs:(19,1)-(24,25) 237          0    0.0   11.9   100.0   64.1
  euclid1            GCD                   src/GCD.hs:(21,1)-(25,44)  238       1024  100.0   43.6   100.0   43.6
  euclid2            GCD                   src/GCD.hs:(29,1)-(32,41)  242          2    0.0    0.1     0.0    0.1
   euclid2.remainder GCD                   src/GCD.hs:32:21-41        243          2    0.0    0.0     0.0    0.0
  eain.(...)         Main                  app/Main.hs:23:19-40       240          1    0.0    8.5     0.0    8.5
  main.u             Main                  app/Main.hs:23:19-40       239          1    0.0    0.0     0.0    0.0
  main.v             Main                  app/Main.hs:23:19-40       241          1    0.0    0.0     0.0    0.0
```

### Using ThreadScope

Another useful tool for performance profiling is
[ThreadScope](https://wiki.haskell.org/ThreadScope).

* compile with multi-threaded runtime:

```bash
ghc -threaded -eventlog -rtsopts --make app/Main.hs src/GCD.hs
```

* execute program and generate a profile use the `-ls` flag after `+RTS`

```bash
app/Main 371 379904 +RTS -ls -N2
```

* pass the profile into ThreadScope:

```bash
threadscope Main.eventlog
```

Here is an example of the report:

![threadscope-main.png](files/threadscope-main.png)

## Project Information

```text
$ cabal info .
Warning: Unknown/unsupported 'ghc' version detected (Cabal 3.6.2.0 supports
'ghc' version < 9.4): /home/frank/.ghcup/bin/ghc is version 9.4.7
Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.
* gcd-0.12.0              (program and library)
    Synopsis:      Greatest Common Denominator
    Versions available: [ Not available from server ]
    Versions installed: [ Not installed ]
    Homepage:      https://github.com/frankhjung/gcd#readme
    Bug reports:   [ Not specified ]
    Description:   Test versions of Euclid's greatest common denominator
                   algorithm
    Category:      education
    License:       GPL-3.0-only
    Author:        Frank H Jung
    Maintainer:    frankhjung@linux.com
    Source repo:   git@github.com:frankhjung/haskell-gcd.git
    Executables:   gcd
    Dependencies:  base, base >=4.13 && <5.0, gcd, base, gcd, hspec, base,
                   criterion, gcd
    Cached:        Yes
    Modules:
        Gcd
```
