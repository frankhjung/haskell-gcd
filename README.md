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

* [lint](http://hackage.haskell.org/package/hlint) and [style](https://hackage.haskell.org/package/stylish-haskell) source code
* build using [GNU Make](https://www.gnu.org/software/make/) using [Stack](https://docs.haskellstack.org)
* unit test with [HSpec](http://hackage.haskell.org/package/hspec)
* benchmark with [Criterion](http://hackage.haskell.org/package/criterion)
* document using [Haddock](http://hackage.haskell.org/package/haddock)
* profile using [GHC](https://www.haskell.org/ghc/)

It uses a couple of Greatest Common Denominator (GCD) algorithms as examples for
testing and benchmarking. The GCD algorithms are described
[here](https://en.wikipedia.org/wiki/Euclidean_algorithm).

## Documentation

API documentation is available from:

* [GitHub Pages](https://frankhjung.github.io/haskell-gcd/)
	* [API](https://frankhjung.github.io/haskell-gcd/)
	* [Benchmark](https://frankhjung.github.io/haskell-gcd/benchmark.html)
* [GitLab Pages](https://frankhjung1.gitlab.io/haskell-gcd)

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
time                 8.776 ns   (8.709 ns .. 8.840 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.754 ns   (8.706 ns .. 8.870 ns)
std dev              223.8 ps   (113.2 ps .. 407.5 ps)
variance introduced by outliers: 42% (moderately inflated)

benchmarking euclid2: /379904
time                 8.772 ns   (8.689 ns .. 8.864 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.737 ns   (8.699 ns .. 8.820 ns)
std dev              181.5 ps   (88.17 ps .. 300.4 ps)
variance introduced by outliers: 33% (moderately inflated)

benchmarking gcd: /379904
time                 8.676 ns   (8.669 ns .. 8.683 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.676 ns   (8.670 ns .. 8.685 ns)
std dev              23.67 ps   (18.23 ps .. 31.02 ps)

Benchmark benchmark: FINISH
```

### Report

* [Benchmark Report](files/benchmark.html)

## Profiling

### Using GHC

The following shows how to profile this application using GHC:

1. compile with profiling

  ```bash
  ghc -prof -fprof-auto -rtsopts app/Main.hs src/GCD.hs
  ```

2. to profile run program with arguments:

```bash
app/Main 371 379904 +RTS -p
```

3. results are reported in `Main.prof`:

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

1. compile with multi-threaded runtime:

```bash
ghc -threaded -eventlog -rtsopts --make app/Main.hs src/GCD.hs
```

2. execute program and generate a profile use the `-ls` flag after `+RTS`

```bash
app/Main 371 379904 +RTS -ls -N2
```

3. pass the profile into ThreadScope:

```bash
threadscope Main.eventlog
```

Here is an example of the report:

![threadscope-main.png](files/threadscope-main.png)

## Project Information

```text
$ cabal info .
Warning: The package list for 'hackage.haskell.org' is 25 days old.
Run 'cabal update' to get the latest list of available packages.
* gcd-0.9.0              (program and library)
    Synopsis:      Greatest Common Denominator
    Versions available: [ Not available from server ]
    Versions installed: [ Not installed ]
    Homepage:      https://github.com/frankhjung/gcd#readme
    Bug reports:   [ Not specified ]
    Description:   Test versions of Euclid's greatest common denominator
                   algorithm
    Category:      education
    License:       GPL-3
    Author:        Frank H Jung
    Maintainer:    frankhjung@linux.com
    Source repo:   git@github.com:frankhjung/haskell-gcd.git
    Executables:   gcd
    Dependencies:  base ==4.13.*, base ==4.13.*, gcd -any, base ==4.13.*,
                   hspec ==2.7.*, gcd -any, base ==4.13.*, criterion ==1.5.*,
                   gcd -any
    Cached:        Yes
    Modules:
        Gcd
```
