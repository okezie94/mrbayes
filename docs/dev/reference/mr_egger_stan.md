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
#> intercept  -0.06    0.00 0.04  -0.15  -0.08  -0.06  -0.04   0.01   195 1.01
#> estimate    4.12    0.16 2.35  -0.16   2.54   3.89   5.53   9.69   210 1.01
#> sigma       7.74    0.08 1.18   5.57   6.84   7.75   8.64   9.80   218 1.02
#> lp__      -35.48    0.08 1.18 -38.47 -35.98 -35.18 -34.63 -34.10   197 1.03
#> 
#> Samples were drawn using NUTS(diag_e) at Mon May  4 11:32:30 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
