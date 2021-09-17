#' Bayesian inverse variance weighted model with a choice of prior distributions fitted using Stan
#'
#' Bayesian inverse variance weighted model with a choice of prior distributions fitted using Stan.
#'
#' @param data A data of class [`mr_format`].
#' @param prior An integer for selecting the prior distributions;
#'
#' * `1` selects a non-informative set of priors;
#' * `2` selects weakly informative priors;
#' * `3` selects a pseudo-horseshoe prior on the causal effect;
#' * `4` selects joint prior of the intercept and causal effect estimate.
#' @param n.chains Numeric indicating the number of chains used in the HMC estimation in rstan, the default is `3` chains.
#' @param n.burn Numeric indicating the burn-in period of the Bayesian HMC estimation. The default is `1000` samples.
#' @param n.iter Numeric indicating the number of iterations in the Bayesian HMC estimation. The default is `5000` iterations.
#' @param rho Numeric indicating the correlation coefficient input into the joint prior distribution. The default is `0.5`.
#' @param seed Numeric indicating the random number seed. The default is `12345`.
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
#' if (requireNamespace("rstan", quietly = TRUE)) {
#' # Note we recommend setting n.burn and n.iter to larger values
#' egger_fit <- mr_egger_stan(bmi_insulin, n.burn = 500, n.iter = 1000)
#' print(egger_fit)
#' }
#' }
mr_egger_stan <- function(data,
                        prior = 1,
                        n.chains = 3,
                        n.burn = 1000,
                        n.iter = 5000,
                        seed = 12345,
                        rho = 0.5,
                        ...) {

  # check for rstan
  rstan_check()

  # convert MRInput object to mr_format
  if ("MRInput" %in% class(data)) {
    data <- mrinput_mr_format(data)
  }

  # check class of object
  if (!("mr_format" %in% class(data))) {
    stop(
      'The class of the data object must be "mr_format", please resave the object with the output of e.g. object <- mr_format(object).'
    )
  }

  pars <- c("intercept","estimate","sigma")

  ## setting directional change

  ybet <- sign(data[,2]) * data[,3]
  xbet <- abs(data[,2])

  # converting dataset to a list
  datam <- list(
    n = nrow(data),
    xbeta = xbet/data[, 5],
    ybeta = ybet/data[, 5],
    weights = 1/data[, 5],
    prior = prior, rho = rho
  )


  eggerfit <- rstan::sampling(
    object = stanmodels$mregger,
    data = datam,
    pars = pars,
    chains = n.chains,
    warmup = n.burn,
    iter = n.iter,
    seed = seed,
    control = list(adapt_delta = 0.999, max_treedepth = 15),
    ...
  )

  return(eggerfit)

}
