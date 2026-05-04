# Bayesian implementation of the MVMR-Egger model with choice of prior distributions fitted using JAGS.

Bayesian implementation of the MVMR-Egger model with choice of prior
distributions fitted using JAGS.

## Usage

``` r
mvmr_egger_rjags(
  object,
  prior = "default",
  betaprior = "",
  sigmaprior = "",
  orientate = 1,
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
  [`mvmr_format`](https://okezie94.github.io/mrbayes/dev/reference/mvmr_format.md).

- prior:

  A character string for selecting the prior distributions;

  - `"default"` selects a non-informative set of priors;

  - `"weak"` selects weakly informative priors;

  - `"pseudo"` selects a pseudo-horseshoe prior on the causal effect;

- betaprior:

  A character string in JAGS syntax to allow a user defined prior for
  the causal effect.

- sigmaprior:

  A character string in JAGS syntax to allow a user defined prior for
  the residual standard deviation.

- orientate:

  Numeric value to indicate the oriented exposure

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

An object of class `mveggerjags` containing the following components:

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

Bowden et. al., Mendelian randomization with invalid instruments: effect
estimation and bias detection through Egger regression. International
Journal of Epidemiology 2015. 44(2): p. 512-525.
[doi:10.1093/ije/dyv080](https://doi.org/10.1093/ije/dyv080)

## Examples

``` r
# \donttest{
if (requireNamespace("rjags", quietly = TRUE)) {
if (FALSE) { # \dontrun{
dat <- mvmr_format(
  rsid = dodata$rsid,
  xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
  ybeta = dodata$chdbeta,
  xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
  yse = dodata$chdse
)

fit <- mvmr_egger_rjags(dat)
summary(fit)
plot(fit$samples)
# 90% credible interval
fitdf <- do.call(rbind.data.frame, fit$samples)
cri90 <- sapply(fitdf, quantile, probs = c(0.05, 0.95))
print(cri90)
} # }
}
# }
```
