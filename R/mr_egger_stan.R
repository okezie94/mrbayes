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
#'
#' @return An object of class [`stanfit`].
#'
#' @references Burgess, S., Butterworth, A., Thompson S.G. Mendelian randomization analysis with multiple genetic variants using summarized data. Genetic Epidemiology, 2013, 37, 7, 658-665 <https://dx.doi.org/10.1002/gepi.21758>.
#'
#' @export
#'
#' @examples
#' egger_fit <- mr_egger_stan(bmi_insulin)
#' print(egger_fit)

mr_egger_stan <- function(data,
                        prior = 1,
                        n.chains = 3,
                        n.burn = 1000,
                        n.iter = 5000,
                        seed = 12345,
                        rho = 0.5) {

  # check class of object
  if (!("mr_format" %in% class(data))) {
    stop(
      'The class of the data object must be "mr_format", please resave the object with the output of e.g. object <- mr_format(object).'
    )
  }

  if (prior == 4){
    pars <- c("eta","sigma")
  } else {pars <- c("intercept","estimate","sigma")}


  # converting dataset to a list
  datam <- list(
    n = nrow(data),
    xbeta = data[, 2]/data[, 5],
    ybeta = data[, 3]/data[, 5],
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
    control = list(adapt_delta = 0.999, max_treedepth = 15)
  )

  return(eggerfit)

}
