#' Bayesian multivariate inverse variance weighted model with a choice of prior distributions fitted using RStan.
#'
#' Bayesian multivariate inverse variance weighted model with a choice of prior distributions fitted using RStan.
#'
#' @param data A data of class [`mvmr_format`].
#' @param prior An integer for selecting the prior distributions;
#'
#' * `1` selects a non-informative set of priors;
#' * `2` selects weakly informative priors;
#' * `3` selects a pseudo-horseshoe prior on the causal effect.
#' @param n.chains Numeric indicating the number of chains used in the HMC estimation in rstan, the default is `3` chains.
#' @param n.burn Numeric indicating the burn-in period of the Bayesian HMC estimation. The default is `1000` samples.
#' @param n.iter Numeric indicating the number of iterations in the Bayesian MCMC estimation. The default is `5000` iterations.
#' @param seed Numeric indicating the random number seed. The default is `12345`.
#' @param ... Additional arguments passed through to [`rstan::sampling()`].
#'
#' @return An object of class [`rstan::stanfit`].
#'
#' @references Burgess, S., Butterworth, A., Thompson S.G. Mendelian randomization analysis with multiple genetic variants using summarized data. Genetic Epidemiology, 2013, 37, 7, 658-665 \doi{10.1002/gepi.21758}.
#' @references Stan Development Team (2020). "RStan: the R interface to Stan." R package version 2.19.3, <https://mc-stan.org/>.
#'
#' @examples
#' if (requireNamespace("rstan", quietly = TRUE)){
#' dat <- mvmr_format(
#'   rsid = dodata$rsid,
#'   xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
#'   ybeta = dodata$chdbeta,
#'   xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
#'   yse = dodata$chdse
#' )
#' mvivw_fit <- mvmr_ivw_stan(dat)
#' print(mvivw_fit)
#' rstan::traceplot(mvivw_fit)
#' }
#' @export
mvmr_ivw_stan <- function(data,
                        prior = 1,
                        n.chains = 3,
                        n.burn = 1000,
                        n.iter = 5000,
                        seed = 12345,
                        ...) {

  # check for rstan
  rstan_check()

  # convert MRInput object to mr_format
  # if ("MRInput" %in% class(data)) {
  #   data <- mrinput_mr_format(data)
  # }

  # check class of object
  if (!("mvmr_format" %in% class(data))) {
    stop(
      'The class of the data object must be "mvmr_format", please resave the object with the output of e.g. object <- mr_format(object).'
    )
  }

  # converting dataset to a list
  datam <- list(
    n = nrow(data$beta.exposure),
    d = ncol(data$beta.exposure),
    xbeta = data$beta.exposure / data$se.outcome,
    ybeta = data$beta.outcome / data$se.outcome,
    prior = prior
  )

  mvivwfit <- rstan::sampling(
    object = stanmodels$mvmrivw,
    data = datam,
    pars = c("estimate"),
    chains = n.chains,
    warmup = n.burn,
    iter = n.iter,
    seed = seed,
    control = list(adapt_delta = 0.999, max_treedepth = 15),
    ...
  )


  return(mvivwfit)

}
