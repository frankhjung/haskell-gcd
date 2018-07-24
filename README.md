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

## Benchmark

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
                   criterion -any, base >=4.7 && <5
    Cached:        Yes
    Modules:
        GCD
```
