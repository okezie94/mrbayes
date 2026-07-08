# mrbayes 0.5.3

* The minimum version of R is now 4.3.0. This is required by the **distributional** package (a transitive dependency via **rstan** → **loo** → **posterior**), which uses `chooseOpsMethod()` (introduced in R 4.3.0).
* Fixes for segfault on macOS ARM runners on r-universe.
* Bump **roxygen2** to 8.0.0.
* Fix hardcoded 3-exposure loop in MVMR-Egger print/summary.
* Fix broken "joint" prior branch in `mvmr_egger_rjags()`.
* Fix sigma parameterization in `mr_radialegger_rjags()`.
* Fix wrong function name in Stan MVMR error messages.
* Complete the sigma < 1 warning message in egger rjags functions.
* Fix `bmi_insulin` column count in documentation.
* Fix comment typos in egger rjags functions.
* Constrain prior to 1-3 in `mvmregger.stan`.
* Remove no-op prior self-assignment in rjags functions.
* Check for rjags before class in MVMR rjags functions.
* Widen non-informative intercept prior in `mregger.stan`.

# mrbayes 0.5.2

* The `NAMESPACE` now imports fewer functions from other packages.

* The number of dependency packages has been reduced.

* Added a `CITATION` file.

* The datasets are now compressed.

* Maintainer switched to TP.

# mrbayes 0.5.1

* The `mvmr_egger_rjags()` helpfile example is now only run if the **rjags** package is installed.

# mrbayes 0.5.0

* The examples in the helpfiles for all functions and in the **testthat** test files now check whether the relevant Bayesian software package is installed (i.e. **rstan** or **rjags**). This is because JAGS does not compile on the aarch64 (Apple M1 processor) architecture.

# mrbayes 0.4.0

* Additional checks for installation of JAGS.

# mrbayes 0.3.0

* Additional functions for multivariate IVW and MR-Egger using JAGS and **rstan**

# mrbayes 0.2.0

* IVW implemented using **rstan**.

* MR-Egger implemented using **rstan**.

* Radial MR-Egger implemented using **rstan**.

# mrbayes 0.1.0

* IVW, MR-Egger, and their radial versions implemented using JAGS.
