# Bayesian inverse variance weighted model with a choice of prior distributions fitted using Stan

Bayesian inverse variance weighted model with a choice of prior
distributions fitted using Stan.

## Usage

``` r
mr_egger_stan(
  data,
  prior = 1,
  n.chains = 3,
  n.burn = 1000,
  n.iter = 5000,
  seed = 12345,
  rho = 0.5,
  ...
)
```

## Arguments

- data:

  A data of class
  [`mr_format`](https://okezie94.github.io/mrbayes/dev/reference/mr_format.md).

- prior:

  An integer for selecting the prior distributions;

  - `1` selects a non-informative set of priors;

  - `2` selects weakly informative priors;

  - `3` selects a pseudo-horseshoe prior on the causal effect;

  - `4` selects joint prior of the intercept and causal effect estimate.

- n.chains:

  Numeric indicating the number of chains used in the HMC estimation in
  rstan, the default is `3` chains.

- n.burn:

  Numeric indicating the burn-in period of the Bayesian HMC estimation.
  The default is `1000` samples.

- n.iter:

  Numeric indicating the number of iterations in the Bayesian HMC
  estimation. The default is `5000` iterations.

- seed:

  Numeric indicating the random number seed. The default is `12345`.

- rho:

  Numeric indicating the correlation coefficient input into the joint
  prior distribution. The default is `0.5`.

- ...:

  Additional arguments passed through to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Value

An object of class
[`rstan::stanfit`](https://mc-stan.org/rstan/reference/stanfit-class.html).

## References

Bowden J, Davey Smith G, Burgess S. Mendelian randomization with invalid
instruments: effect estimation and bias detection through Egger
regression. International Journal of Epidemiology, 2015, 44, 2, 512-525.
[doi:10.1093/ije/dyv080](https://doi.org/10.1093/ije/dyv080) .

Stan Development Team (2020). "RStan: the R interface to Stan." R
package version 2.19.3, <https://mc-stan.org/>.

## Examples

``` r
# \donttest{
if (requireNamespace("rstan", quietly = TRUE)) {
# Note we recommend setting n.burn and n.iter to larger values
suppressWarnings(egger_fit <- mr_egger_stan(bmi_insulin, n.burn = 500, n.iter = 1000, refresh = 0L))
print(egger_fit)
}
#> Inference for Stan model: mregger.
#> 3 chains, each with iter=1000; warmup=500; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1500.
#> 
#>             mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> intercept  -0.05    0.00 0.03  -0.12  -0.07  -0.05  -0.03   0.01   303 1.01
#> estimate    3.64    0.12 2.02  -0.32   2.23   3.72   4.98   7.34   280 1.01
#> sigma       7.61    0.06 1.19   5.43   6.72   7.56   8.56   9.75   339 1.01
#> lp__      -35.30    0.05 0.97 -37.73 -35.81 -35.08 -34.58 -34.10   451 1.01
#> 
#> Samples were drawn using NUTS(diag_e) at Wed Sep 30 10:15:25 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
