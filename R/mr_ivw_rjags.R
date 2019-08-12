#' Fitting Bayesian inverse variance weighted estimates using established priors using the JAGS software.
#'
#' @param object The data frame converted into the format_mr format
#' @param prior The option for selecting the proposed priors; "default" indicates non-informative prior; "weak" indicates weakly informative prior; "pseudo" indicates pseudo-horseshoe prior.
#' @param betaprior This is an option for setting a prior for the causal estimate.
#' @param n.chains This is an option for choosing the number of chains for MCMC simulation, default number is 3 chains.
#' @param n.burn This is the option for the burn in period of the bayesian MCMC runs. The default option is 1000 samples
#' @param n.iter This is the option for the number of bayesian MCMC runs. The default is 5000 iterations
#' @param seed This is an option for setting seeds for reproducible results. The default is to use the rjags default of using the current system time.
#' @param ... Passing options through to rjags::jags.model()
#'
#' @export
#' @return The result object of class ivwjags contains the following components:
#' \describe{
#' \item{CausalEffect}{The mean of the generated causal effects}
#' \item{StandardError}{Standard deviation of the mean causal effect}
#' \item{CredibleInterval}{The credible interval for the causal effect, which indicates the lower(2.5\%), median (50\%) and upper intervals (97.5\%)}
#' \item{samples}{Output of the bayesian MCMC samples with the different chains}
#' }
#'
#' @references Burgess, S., Butterworth, A., Thompson S.G. Mendelian randomization analysis with multiple genetic variants using summarized data. Genetic Epidemiology, 2013, 37, 7, 658-665 <https://dx.doi.org/10.1002/gepi.21758>.
#'
#' @examples
#' data(bmi_insulin)
#' fit <- mr_ivw_rjags(bmi_insulin, n.chains = 1)
#' print(fit)
#' summary(fit)
#' plot(fit$samples)
#'
mr_ivw_rjags <- function(object,
                         prior = "default",
                         betaprior = "",
                         n.chains = 1,
                         n.burn = 1000,
                         n.iter = 5000,
                         seed = NULL,
                         ...) {

  # check class of object
  if (!("mr_format" %in% class(object))) {
    stop('The class of the data object must be "mr_format", please resave the object with the output of e.g. object <- mr_format(object).')
    }

  Likelihood <-
    "for (i in 1:N){
      by[i] ~ dnorm(by.hat[i], tau[i])
      by.hat[i] <- Estimate * bx[i]
      tau[i] <- pow(byse[i], -2)
    }"


  if (prior == "default" & betaprior == "") {
    #Setting up the model string

    Priors <- "Estimate ~ dnorm(0, 1E-3)"
    ivw_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")


} else if (prior == "weak" & betaprior == "") {
    #Setting up the model string
    Priors<- "Estimate ~ dnorm(0, 1E-6)"

    ivw_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")

} else if (prior == "pseudo" & betaprior == "") {

    #Setting up the model string
  Priors<- "Estimate ~ dt(0, 1, 1)"
  ivw_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")
}

if (betaprior != "") {

  Priors<- paste0("Estimate ~ ",betaprior)
  ivw_model_string <- paste0("model {", Likelihood,"\n\n", Priors, "\n\n }")

}

  if (!is.null(seed)) {
    if (length(seed) != n.chains) {
      stop('The length of the seed vector must be equal to the number of chains.')
    }

    initsopt <- list()
    for (i in 1:n.chains) {
      initsopt[[i]] <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = seed[i])
    }
  } else {
    initsopt <- NULL
  }

ivw_model <- rjags::jags.model(
  textConnection(ivw_model_string),
  data = list(
    N = nrow(object),
    by = object[, 3],
    bx = object[, 2],
    byse = object[, 5]
  ),
  n.chains = n.chains,
  inits = initsopt,
  ...
)
# Burn-in
update.jags <- utils::getFromNamespace("update.jags", "rjags")
update.jags(ivw_model, n.iter = n.burn)




# Collect samples
ivw_samp <- rjags::coda.samples(ivw_model,
                                variable.names = c("Estimate"),
                                n.iter = n.iter)

# model and parameters for sampling
g <- ivw_samp

p <- summary(ivw_samp)

prior <- prior

niter <- n.iter

nburn <- n.burn

nchain <- n.chains

nsnps <- nrow(object)

mcmciter <- n.iter + n.burn

# Outputs from the model

#Causal Estimate
causal.est <- p$statistics[1]

#standard deviation
standard.dev <- p$statistics[2]

#lower Credible Interval for estimates
lower.credible_interval <- p$quantiles[1]

#Median Interval for estimates
Median_interval <- p$quantiles[3]

#higher Credible Interval for estimates
Higher.credible_interval <- p$quantiles[5]

credible_interval <-
  c(lower.credible_interval,
    Median_interval,
    Higher.credible_interval)

#Class for the output
out <- list()
out$CausalEffect <- causal.est
out$StandardError <- standard.dev
out$CredibleInterval <- credible_interval
out$samples <- g
out$priormethod <- prior
out$betaprior <- betaprior
out$samplesize<- niter
out$burnin<- nburn
out$chains<- nchain
out$MCMC<- mcmciter
out$Prior <- Priors
out$model <- ivw_model_string
out$nsnps<- nsnps

class(out) <- "ivwjags"
return(out)

}

#Function for output of results
#' @export
print.ivwjags <- function(x, ...) {
  outt <-
    matrix(c(x$CausalEffect, x$StandardError, x$CredibleInterval),
           nrow = 1,
           ncol = 5,
           byrow = TRUE,
           dimnames = list("Causal Effect", c("Estimate", "SD", "2.5%", "50%", "97.5%"))
    )
  print(outt, ...)
  invisible(x)
}

# Generating a summary of the results
#' @export
summary.ivwjags <- function(object, ...) {
  out <- object
  out1 <-
    matrix(
      c(out$CausalEffect, out$StandardError, out$CredibleInterval),
      nrow = 1,
      ncol = 5,
      byrow = TRUE,
      dimnames = list("Causal Effect", c("Estimate", "SD", "2.5%", "50%", "97.5%"))
    )

  cat("Prior : \n\n", out$Prior, "\n\n")
  cat("Estimation results:", "\n", "\n")
  cat(DescTools::StrAlign("MCMC iterations = ", "\\r"),
      out$MCMC,
      "\n")
  cat(DescTools::StrAlign("Burn in = ", "\\r"), out$burnin, "\n")
  cat(DescTools::StrAlign("Sample size by chain = ", "\\r"),
      out$samplesize,
      "\n")
  cat(DescTools::StrAlign("Number of Chains = ", "\\r"),
      out$chains,
      "\n")
  cat(DescTools::StrAlign("Number of SNPs = ", "\\r"),
      out$nsnps,
      "\n",
      "\n")

  print(out1, ...)

}
