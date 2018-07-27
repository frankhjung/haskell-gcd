# Haskell Example Project

This is an example project to show how to:

- lint, style and check source code
- build using GNU Make using Cabal or Stack
- unit test with HSpec
- benchmark with Criterion
- document using Haddock
- profile using GHC

It uses a couple of Greatest Common Denominator (GCD) algorithms as examples for
testing and benchmarking. The GCD algorithms are described
[here](https://en.wikipedia.org/wiki/Euclidean_algorithm).

## Build

```bash
make -f cabal.mk build
```

Or

```bash
make -f stack.mk build
```

## Tests

Running tests using Cabal:

```
cat dist/test/GCD-0.1.0-test.log
Test suite test: RUNNING...

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

Finished in 0.0010 seconds
4 examples, 0 failures
Test suite test: PASS
Test suite logged to: dist/test/GCD-0.1.0-test.log
```

## Benchmarks

Benchmark of the two Euclid _Greatest Common Denominator_ algorithms.

```bash
$ stack bench
GCD-0.1.0: benchmarks
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking euclid 1 subtraction/317
time                 8.013 ns   (7.992 ns .. 8.035 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 8.002 ns   (7.977 ns .. 8.026 ns)
std dev              77.61 ps   (62.01 ps .. 100.1 ps)

benchmarking euclid 2 modulus/317
time                 8.001 ns   (7.981 ns .. 8.022 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.983 ns   (7.962 ns .. 8.003 ns)
std dev              67.93 ps   (56.92 ps .. 83.74 ps)

Benchmark benchmark: FINISH
```

## Profiling Using GHC

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

## Dependencies

### Install Dependencies

Install test, benchmark frameworks and an external GCD library:

```bash
cabal install hspec
cabal install criterion
cabal install --allow-newer besout-0.2.0.1
```

### List Dependencies

```bash
$ stack list-dependencies
GCD 0.1.0
base 4.11.1.0
ghc-prim 0.5.2.0
integer-gmp 1.0.2.0
rts 1.0
```

## Project Information

```bash
$ cabal info .
* GCD-0.1.0        (program and library)
    Synopsis:      Greatest Common Denominator
    Homepage:      https://github.com/frankhjung/gcd#readme
    Description:   Test versions of Euclid's greatest common denominator
                   algorithm
    Category:      education
    License:       GPL-3
    Author:        Frank H Jung
    Maintainer:    frankhjung at linux.com
    Source repo:   git@github.com:frankhjung/haskell-gcd.git
    Executables:   gcd
    Dependencies:  base >=4.7 && <5, GCD -any, base >=4.7 && <5, GCD -any,
                   hspec >=2.5, base >=4.7 && <5, GCD -any, criterion >=1.3,
                   base >=4.7 && <5
    Cached:        Yes
    Modules:       GCD
```
