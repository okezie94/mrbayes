# Changelog

## mrbayes (development version)

- The minimum version of R is now 4.2.0 due to the requirements of the
  **DescTools** package.
- Fixes for segfault on macOS ARM runners on r-universe.
- Bump **roxygen2** to 8.0.0.

## mrbayes 0.5.2

CRAN release: 2024-08-19

- The `NAMESPACE` now imports fewer functions from other packages.

- The number of dependency packages has been reduced.

- Added a `CITATION` file.

- The datasets are now compressed.

- Maintainer switched to TP.

## mrbayes 0.5.1

CRAN release: 2021-10-02

- The
  [`mvmr_egger_rjags()`](https://okezie94.github.io/mrbayes/dev/reference/mvmr_egger_rjags.md)
  helpfile example is now only run if the **rjags** package is
  installed.

## mrbayes 0.5.0

CRAN release: 2021-09-21

- The examples in the helpfiles for all functions and in the
  **testthat** test files now check whether the relevant Bayesian
  software package is installed (i.e. **rstan** or **rjags**). This is
  because JAGS does not compile on the aarch64 (Apple M1 processor)
  architecture.

## mrbayes 0.4.0

CRAN release: 2021-07-13

- Additional checks for installation of JAGS.

## mrbayes 0.3.0

CRAN release: 2021-04-01

- Additional functions for multivariate IVW and MR-Egger using JAGS and
  **rstan**

## mrbayes 0.2.0

CRAN release: 2020-05-28

- IVW implemented using **rstan**.

- MR-Egger implemented using **rstan**.

- Radial MR-Egger implemented using **rstan**.

## mrbayes 0.1.0

CRAN release: 2019-09-04

- IVW, MR-Egger, and their radial versions implemented using JAGS.
