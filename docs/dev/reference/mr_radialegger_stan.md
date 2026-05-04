# Bayesian inverse variance weighted model with a choice of prior distributions fitted using RStan.

Bayesian inverse variance weighted model with a choice of prior
distributions fitted using RStan

## Usage

``` r
mr_radialegger_stan(
  data,
  prior = 1,
  n.chains = 3,
  n.burn = 1000,
  n.iter = 5000,
  rho = 0.5,
  seed = 12345,
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

- rho:

  Numeric indicating the correlation coefficient input into the joint
  prior distribution. The default is `0.5`.

- seed:

  Numeric indicating the random number seed. The default is `12345`.

- ...:

  Additional arguments passed through to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Value

An object of class
[`rstan::stanfit`](https://mc-stan.org/rstan/reference/stanfit-class.html).

## References

Bowden, J., et al., Improving the visualization, interpretation and
analysis of two-sample summary data Mendelian randomization via the
Radial plot and Radial regression. International Journal of
Epidemiology, 2018. 47(4): p. 1264-1278.
[doi:10.1093/ije/dyy101](https://doi.org/10.1093/ije/dyy101) .

Stan Development Team (2020). "RStan: the R interface to Stan." R
package version 2.19.3, <https://mc-stan.org/>.

## Examples

``` r
# \donttest{
if (requireNamespace("rstan", quietly = TRUE)) {
# Note we recommend setting n.burn and n.iter to larger values
suppressWarnings({
  radegger_fit <- mr_radialegger_stan(bmi_insulin, n.burn = 500, n.iter = 1000, refresh = 0L)
})
print(radegger_fit)
}
#> Inference for Stan model: mrradialegger.
#> 3 chains, each with iter=1000; warmup=500; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1500.
#> 
#>             mean se_mean    sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> intercept -30.27    0.69 13.02 -55.63 -38.65 -30.91 -21.49  -4.76   360 1.00
#> estimate    6.31    0.13  2.48   1.34   4.67   6.44   7.95  11.14   371 1.00
#> sigma       7.03    0.07  1.29   4.87   6.04   6.91   7.93   9.59   325 1.02
#> lp__      -33.88    0.05  1.10 -36.56 -34.50 -33.67 -33.03 -32.47   425 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Mon May  4 11:32:32 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
