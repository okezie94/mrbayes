# mrbayes 0.5.2

* The NAMESPACE now imports fewer functions from other packages

* The number of dependency packages has been reduced

* Added a `CITATION` file

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
