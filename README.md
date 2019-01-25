# Haskell Example Project

This is an example project to show how to:

- lint, style and check source code
- build using GNU Make using Cabal or Stack
- unit test with [HSpec](http://hackage.haskell.org/package/hspec)
- benchmark with [Criterion](http://hackage.haskell.org/package/criterion)
- document using [Haddock](http://hackage.haskell.org/package/haddock)
- profile using [GHC](https://www.haskell.org/ghc/)

It uses a couple of Greatest Common Denominator (GCD) algorithms as examples for
testing and benchmarking. The GCD algorithms are described
[here](https://en.wikipedia.org/wiki/Euclidean_algorithm).

## Build

To prime environment with compiler, packages and dependencies, run the
environment setup:

```bash
$ stack update
$ stack setup
$ stack build
$ cabal configure
$ cabal build
```

This may not include the linter, styler and documentation tools.
TODO - can we add this to stack.yaml as external dependencies?

Then build using [GNU Make](https://www.gnu.org/software/make/):

```bash
$ make -f stack.mk build
```

Or

```bash
$ make -f cabal.mk build
```

Stack uses Cabal under the covers but does more package management. However,
once Stack has installed required packages, then Cabal should happily compile
for you.

## Tests

Running tests using Cabal:

```
$ stack test

GCD-0.3.0: test (suite: test)
            
Progress 1/2: GCD-0.3.0
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
besout
  besout 1 1
    returns 1
  besout 371 379904
    returns 371

Finished in 0.0014 seconds
6 examples, 0 failures
                       
GCD-0.3.0: Test suite test passed
Generating coverage report for GCD's test-suite "test"
 78% expressions used (29/37)
 33% boolean coverage (2/6)
      20% guards (1/5), 1 always True, 2 always False, 1 unevaluated
     100% 'if' conditions (1/1)
     100% qualifiers (0/0)
 57% alternatives used (4/7)
100% local declarations used (1/1)
100% top-level declarations used (2/2)
```

## Benchmarks

Benchmark the two Euclid _Greatest Common Denominator_ algorithms:

```bash
$ stack bench

GCD-0.3.0: benchmarks
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking euclid1: /379904
time                 7.376 ns   (7.351 ns .. 7.413 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 7.397 ns   (7.370 ns .. 7.455 ns)
std dev              127.8 ps   (84.63 ps .. 198.1 ps)
variance introduced by outliers: 25% (moderately inflated)
                       
benchmarking euclid2: /379904
time                 7.403 ns   (7.369 ns .. 7.445 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 7.427 ns   (7.400 ns .. 7.497 ns)
std dev              143.7 ps   (74.44 ps .. 282.4 ps)
variance introduced by outliers: 30% (moderately inflated)
                       
benchmarking besout: /379904
time                 7.391 ns   (7.368 ns .. 7.419 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.403 ns   (7.381 ns .. 7.434 ns)
std dev              83.06 ps   (61.34 ps .. 126.2 ps)
variance introduced by outliers: 12% (moderately inflated)
                       
Benchmark benchmark: FINISH
```

## Profiling Using GHC

The following shows how to profile this application using GHC:

1. compile with profiling

```bash
$ ghc -prof -fprof-auto -rtsopts app/Main.hs src/GCD.hs
```

2. to profile run program with arguments:

```bash
$ app/Main 371 379904 +RTS -p
```

3. results are reported in `Main.prof`:

```
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


                                                                                         individual      inherited
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
  main.(...)         Main                  app/Main.hs:23:19-40       240          1    0.0    8.5     0.0    8.5
  main.u             Main                  app/Main.hs:23:19-40       239          1    0.0    0.0     0.0    0.0
  main.v             Main                  app/Main.hs:23:19-40       241          1    0.0    0.0     0.0    0.0
```

## Profiling Using ThreadScope

Another useful tool for performance profiling is
[ThreadScope](https://wiki.haskell.org/ThreadScope).

1. compile with multi-threaded runtime:

```bash
$ ghc -threaded -eventlog -rtsopts --make app/Main.hs src/GCD.hs
```

2. execute program and generate a profile use the `-ls` flag after `+RTS`. 

```bash
$ app/Main 371 379904 +RTS -ls -N2
```

3. pass the profile into ThreadScope:

```bash
$ threadscope Main.eventlog
```

The following is example output for this process:

![threadscope-main.png](./files/threadscope-main.png)

## Dependencies

### Install Dependencies

Install test, benchmark frameworks and an external GCD library:

```bash
$ cabal install hspec
$ cabal install criterion
$ cabal install --allow-newer besout-0.2.0.1
```

### List Dependencies

```bash
$ stack ls dependencies

GCD 0.3.0
base 4.11.1.0
ghc-prim 0.5.2.0
integer-gmp 1.0.2.0
rts 1.0
```

## Project Information

```bash
$ cabal info .

* GCD-0.3.0              (program and library)
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
    Dependencies:  base >=4.11.1 && <4.12, GCD -any, base >=4.11.1 && <4.12,
                   GCD -any, hspec >=2.5.6, besout ==0.2.0.1,
                   base >=4.11.1 && <4.12, GCD -any, criterion >=1.4.1 && <1.6,
                   besout -any, base >=4.11.1 && <4.12
    Cached:        Yes
    Modules:
        GCD
```
