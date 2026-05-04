# Bayesian implementation of the MVMR-Egger model with choice of prior distributions fitted using RStan.

Bayesian implementation of the MVMR-Egger model with choice of prior
distributions fitted using RStan.

## Usage

``` r
mvmr_egger_stan(
  data,
  prior = 1,
  n.chains = 3,
  n.burn = 1000,
  n.iter = 5000,
  seed = 12345,
  rho = 0.5,
  orientate = 1,
  ...
)
```

## Arguments

- data:

  A data of class
  [`mvmr_format`](https://okezie94.github.io/mrbayes/dev/reference/mvmr_format.md).

- prior:

  An integer for selecting the prior distributions;

  - `1` selects a non-informative set of priors;

  - `2` selects weakly informative priors;

  - `3` selects a pseudo-horseshoe prior on the causal effect;

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

- orientate:

  Numeric value to indicate the oriented exposure.

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
dat <- mvmr_format(
  rsid = dodata$rsid,
  xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
  ybeta = dodata$chdbeta,
  xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
  yse = dodata$chdse
)
suppressWarnings(mvegger_fit <- mvmr_egger_stan(dat, n.burn = 500, n.iter = 1000, refresh = 0L))
print(mvegger_fit)
}
#> Inference for Stan model: mvmregger.
#> 3 chains, each with iter=1000; warmup=500; thin=1; 
#> post-warmup draws per chain=500, total post-warmup draws=1500.
#> 
#>                mean se_mean   sd    2.5%     25%     50%     75%   97.5% n_eff
#> intercept     -0.01    0.00 0.00   -0.01   -0.01   -0.01    0.00    0.00  1287
#> estimate[1]    0.53    0.00 0.07    0.38    0.48    0.52    0.58    0.67   928
#> estimate[2]   -0.10    0.00 0.06   -0.23   -0.14   -0.10   -0.06    0.02   895
#> estimate[3]    0.33    0.00 0.08    0.18    0.28    0.33    0.38    0.48  1009
#> sigma          1.46    0.00 0.08    1.31    1.41    1.46    1.51    1.63   978
#> lp__        -161.84    0.07 1.66 -166.00 -162.80 -161.51 -160.59 -159.63   571
#>             Rhat
#> intercept      1
#> estimate[1]    1
#> estimate[2]    1
#> estimate[3]    1
#> sigma          1
#> lp__           1
#> 
#> Samples were drawn using NUTS(diag_e) at Mon May  4 11:32:33 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
# }
```
