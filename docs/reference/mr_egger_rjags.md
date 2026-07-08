# Bayesian implementation of the MR-Egger multivariate model with choice of prior distributions fitted using JAGS.

Bayesian implementation of the MR-Egger multivariate model with choice
of prior distributions fitted using JAGS.

## Usage

``` r
mr_egger_rjags(
  object,
  prior = "default",
  betaprior = "",
  sigmaprior = "",
  n.chains = 3,
  n.burn = 1000,
  n.iter = 5000,
  seed = NULL,
  rho = 0.5,
  ...
)
```

## Arguments

- object:

  A data object of class
  [`mr_format`](https://okezie94.github.io/mrbayes/reference/mr_format.md).

- prior:

  A character string for selecting the prior distributions;

  - `"default"` selects a non-informative set of priors;

  - `"weak"` selects weakly informative priors;

  - `"pseudo"` selects a pseudo-horseshoe prior on the causal effect;

  - `"joint"` selects a joint prior on the intercept and slope.

- betaprior:

  A character string in JAGS syntax to allow a user defined prior for
  the causal effect.

- sigmaprior:

  A character string in JAGS syntax to allow a user defined prior for
  the residual standard deviation.

- n.chains:

  Numeric indicating the number of chains used in the MCMC estimation,
  the default is `3` chains.

- n.burn:

  Numeric indicating the burn-in period of the Bayesian MCMC estimation.
  The default is `1000` samples.

- n.iter:

  Numeric indicating the number of iterations in the Bayesian MCMC
  estimation. The default is `5000` iterations.

- seed:

  Numeric indicating the random number seed. The default is the rjags
  default.

- rho:

  Numeric indicating the correlation coefficient input into the joint
  prior distribution. The default value is `0.5`.

- ...:

  Additional arguments passed through to
  [`rjags::jags.model()`](https://rdrr.io/pkg/rjags/man/jags.model.html).

## Value

An object of class `eggerjags` containing the following components:

- AvgPleio:

  The mean of the simulated pleiotropic effect

- CausalEffect:

  The mean of the simulated causal effect

- StandardError:

  Standard deviation of the simulated causal effect

- sigma:

  The value of the residual standard deviation

- CredibleInterval:

  The credible interval for the causal effect, which includes the lower
  (2.5%), median (50%) and upper intervals (97.5%)

- samples:

  Output of the Bayesian MCMC samples

- Priors:

  The specified priors

## References

Bowden et al., Mendelian randomization with invalid instruments: effect
estimation and bias detection through Egger regression. International
Journal of Epidemiology 2015. 44(2): p. 512-525.
[doi:10.1093/ije/dyv080](https://doi.org/10.1093/ije/dyv080)

## Examples

``` r
# \donttest{
if (requireNamespace("rjags", quietly = TRUE)) {
fit <- mr_egger_rjags(bmi_insulin)
summary(fit)
plot(fit$samples)
# 90% credible interval
fitdf <- do.call(rbind.data.frame, fit$samples)
cri90 <- sapply(fitdf, quantile, probs = c(0.05, 0.95))
print(cri90)
}
#> Prior : 
#> 
#>  Pleiotropy ~ dnorm(0, 1E-3) 
#>  Estimate ~ dnorm(0, 1E-3) 
#>  sigma ~ dunif(.0001, 10) 
#> 
#> Estimation results: 
#>  
#>  MCMC iterations = 6000 
#>  Burn in = 1000 
#>  Sample size by chain = 5000 
#>  Number of Chains = 3 
#>  Number of SNPs = 14 
#>  
#> Inflating Parameter: 7.664701 
#> 
#>                  Estimate         SD       2.5%         50%      97.5%
#> Avg Pleio     -0.05533446 0.03897114 -0.1308351 -0.05478112 0.02263803
#> Causal Effect  3.81386257 2.31376234 -0.8042478  3.78257739 8.34389508

#>       Estimate   Pleiotropy    sigma
#> 5%  0.09344008 -0.117751073 5.711279
#> 95% 7.51835862  0.007370111 9.640371
# }
```
