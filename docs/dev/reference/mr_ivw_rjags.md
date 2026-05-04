# Bayesian inverse variance weighted model with a choice of prior distributions fitted using JAGS.

Bayesian inverse variance weighted model with a choice of prior
distributions fitted using JAGS.

## Usage

``` r
mr_ivw_rjags(
  object,
  prior = "default",
  betaprior = "",
  n.chains = 3,
  n.burn = 1000,
  n.iter = 5000,
  seed = NULL,
  ...
)
```

## Arguments

- object:

  A data object of class
  [`mr_format`](https://okezie94.github.io/mrbayes/dev/reference/mr_format.md).

- prior:

  A character string for selecting the prior distributions;

  - `"default"` selects a non-informative set of priors;

  - `"weak"` selects weakly informative priors;

  - `"pseudo"` selects a pseudo-horseshoe prior on the causal effect.

- betaprior:

  A character string in JAGS syntax to allow a user defined prior for
  the causal effect.

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

- ...:

  Additional arguments passed through to
  [`rjags::jags.model()`](https://rdrr.io/pkg/rjags/man/jags.model.html).

## Value

An object of class `ivwjags` containing the following components:

- CausalEffect:

  The mean of the simulated causal effects

- StandardError:

  Standard deviation of the simulated causal effects

- CredibleInterval:

  The credible interval for the causal effect, which indicates the lower
  (2.5%), median (50%) and upper intervals (97.5%)

- samples:

  Output of the Bayesian MCMC samples with the different chains

- Priors:

  The specified priors

## References

Burgess, S., Butterworth, A., Thompson S.G. Mendelian randomization
analysis with multiple genetic variants using summarized data. Genetic
Epidemiology, 2013, 37, 7, 658-665
[doi:10.1002/gepi.21758](https://doi.org/10.1002/gepi.21758) .

## Examples

``` r
# \donttest{
if (requireNamespace("rjags", quietly = TRUE)) {
fit <- mr_ivw_rjags(bmi_insulin)
print(fit)
summary(fit)
plot(fit$samples)
# 90% credible interval
fitdf <- do.call(rbind.data.frame, fit$samples)
cri90 <- quantile(fitdf$Estimate, probs = c(0.05, 0.95))
print(cri90)
}
#>                Estimate        SD     2.5%       50%     97.5%
#> Causal Effect 0.5793659 0.0509007 0.478432 0.5794962 0.6789618
#> Prior : 
#> 
#>  Estimate ~ dnorm(0, 1E-3) 
#> 
#> Estimation results: 
#>  
#>  MCMC iterations = 6000 
#>  Burn in = 1000 
#>  Sample size by chain = 5000 
#>  Number of Chains = 3 
#>  Number of SNPs = 14 
#>  
#>                Estimate        SD     2.5%       50%     97.5%
#> Causal Effect 0.5793659 0.0509007 0.478432 0.5794962 0.6789618

#>        5%       95% 
#> 0.4950446 0.6627057 
# }
```
