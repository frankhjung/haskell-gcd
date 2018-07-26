# GCD

Project to benchmark Greatest Common Denominator algorithms.

The two methods tested here are described
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

## Dependencies

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
* GCD-0.1.0              (program and library)
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
    Maintainer:    frankhjung at linux.com
    Source repo:   git@github.com:frankhjung/haskell-gcd.git
    Executables:   gcd
    Dependencies:  base >=4.7 && <5, GCD -any, base >=4.7 && <5, GCD -any,
                   hspec >=2.5, base >=4.7 && <5, GCD -any, criterion >=1.3,
                   base >=4.7 && <5
    Cached:        Yes
    Modules:
        GCD
```
