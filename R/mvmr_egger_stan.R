#' Bayesian implementation of the MVMR-Egger model with choice of prior distributions fitted using RStan.
#'
#' Bayesian implementation of the MVMR-Egger model with choice of prior distributions fitted using RStan.
#'
#' @param data A data of class [`mvmr_format`].
#' @param prior An integer for selecting the prior distributions;
#'
#' * `1` selects a non-informative set of priors;
#' * `2` selects weakly informative priors;
#' * `3` selects a pseudo-horseshoe prior on the causal effect;
#' @param n.chains Numeric indicating the number of chains used in the HMC estimation in rstan, the default is `3` chains.
#' @param n.burn Numeric indicating the burn-in period of the Bayesian HMC estimation. The default is `1000` samples.
#' @param n.iter Numeric indicating the number of iterations in the Bayesian HMC estimation. The default is `5000` iterations.
#' @param rho Numeric indicating the correlation coefficient input into the joint prior distribution. The default is `0.5`.
#' @param seed Numeric indicating the random number seed. The default is `12345`.
#' @param orientate Numeric value to indicate the oriented exposure.
#' @param ... Additional arguments passed through to [`rstan::sampling()`].
#'
#' @return An object of class [`stanfit`].
#'
#' @references Bowden J, Davey Smith G, Burgess S. Mendelian randomization with invalid instruments: effect estimation and bias detection through Egger regression. International Journal of Epidemiology, 2015, 44, 2, 512-525. \doi{10.1093/ije/dyv080}.
#' @references Stan Development Team (2020). "RStan: the R interface to Stan." R package version 2.19.3, <https://mc-stan.org/>.
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (require("rstan")) {
#' # Note we recommend setting n.burn and n.iter to larger values
#' dat <- mvmr_format(
#'   rsid = dodata$rsid,
#'   xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
#'   ybeta = dodata$chdbeta,
#'   xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
#'   yse = dodata$chdse
#' )
#' mvegger_fit <- mvmr_egger_stan(dat, n.burn = 500, n.iter = 1000)
#' print(mvegger_fit)
#' }
#' }
mvmr_egger_stan <- function(data,
                        prior = 1,
                        n.chains = 3,
                        n.burn = 1000,
                        n.iter = 5000,
                        seed = 12345,
                        rho = 0.5,
                        orientate = 1,
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

  # orientation setup

  if (orientate %in% 1:dim(data$beta.exposure)[2]) {
    orientAte = orientate
  } else {
    orientAte = 1
  }

  pars <- c("intercept","estimate","sigma")

  ## setting directional change

  orient <- sign(data$beta.exposure)[,orientAte]
  ybet <- orient * data$beta.outcome
  xbet <- orient * data$beta.exposure


  # converting dataset to a list
  datam <- list(
    n = nrow(data$beta.exposure),
    d = ncol(data$beta.exposure),
    xbeta = xbet/data$se.outcome,
    ybeta = ybet/data$se.outcome,
    weights = 1/data$se.outcome,
    prior = prior, rho = rho
  )


  mveggerfit <- rstan::sampling(
    object = stanmodels$mvmregger,
    data = datam,
    pars = pars,
    chains = n.chains,
    warmup = n.burn,
    iter = n.iter,
    seed = seed,
    control = list(adapt_delta = 0.999, max_treedepth = 15),
    ...
  )

  return(mveggerfit)

}
