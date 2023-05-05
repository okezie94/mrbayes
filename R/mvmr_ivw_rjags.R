#' Bayesian multivariate inverse variance weighted model with a choice of prior distributions fitted using JAGS.
#'
#' Bayesian multivariate inverse variance weighted model with a choice of prior distributions fitted using JAGS.
#'
#' @param object A data object of class [`mvmr_format`].
#' @param prior A character string for selecting the prior distributions;
#'
#' * `"default"` selects a non-informative set of priors;
#' * `"weak"` selects weakly informative priors;
#' * `"pseudo"` selects a pseudo-horseshoe prior on the causal effect.
#' @param betaprior A character string in JAGS syntax to allow a user defined prior for the causal effect.
#' @param n.chains Numeric indicating the number of chains used in the MCMC estimation, the default is `3` chains.
#' @param n.burn Numeric indicating the burn-in period of the Bayesian MCMC estimation. The default is `1000` samples.
#' @param n.iter Numeric indicating the number of iterations in the Bayesian MCMC estimation. The default is `5000` iterations.
#' @param seed Numeric indicating the random number seed. The default is the rjags default.
#' @param ... Additional arguments passed through to [`rjags::jags.model()`].
#'
#' @export
#' @return An object of class `mvivwjags` containing the following components:
#' \describe{
#' \item{CausalEffect}{The mean of the simulated causal effects}
#' \item{StandardError}{Standard deviation of the simulated causal effects}
#' \item{CredibleInterval}{The credible interval for the causal effect, which indicates the lower (2.5%), median (50%) and upper intervals (97.5%)}
#' \item{samples}{Output of the Bayesian MCMC samples with the different chains}
#' \item{Priors}{The specified priors}
#' }
#'
#' @references Burgess, S., Butterworth, A., Thompson S.G. Mendelian randomization analysis with multiple genetic variants using summarized data. Genetic Epidemiology, 2013, 37, 7, 658-665 \doi{10.1002/gepi.21758}.
#'
#' @examples
#' if (require("rjags", quietly = TRUE)) {
#' dat <- mvmr_format(
#'   rsid = dodata$rsid,
#'   xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
#'   ybeta = dodata$chdbeta,
#'   xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
#'   yse = dodata$chdse
#' )
#'
#' fit <- mvmr_ivw_rjags(dat)
#' print(fit)
#' summary(fit)
#' plot(fit$samples)
#' # 90% credible interval
#' fitdf <- do.call(rbind.data.frame, fit$samples)
#' cri90 <- sapply(fitdf, quantile, probs = c(0.05, 0.95))
#' print(cri90)
#' }
mvmr_ivw_rjags <- function(object,
                         prior = "default",
                         betaprior = "",
                         n.chains = 3,
                         n.burn = 1000,
                         n.iter = 5000,
                         seed = NULL,
                         ...) {

  # convert MRInput object to mvmr_format
  # if ("MRInput" %in% class(object)) {
  #   object <- mrinput_mvmr_format(object)
  # }

  # check class of object
  if (!("mvmr_format" %in% class(object))) {
    stop('The class of the data object must be "mvmr_format", please resave the object with the output of e.g. object <- mvmr_format(object).')
  }

  # check if rjags is installed
  rjags_check()

  Likelihood <-
    "for (i in 1:N){
      by[i] ~ dnorm(by.hat[i], tau[i])
      by.hat[i] <- inprod(Estimate[], bx[i,])
      tau[i] <- pow(byse[i], -2)
    }"


  if (prior == "default" & betaprior == "") {
    #Setting up the model string

    Priors <- "for (j in 1:K) {
  Estimate[j] ~ dnorm(0,1E-3)
  }"
    ivw_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")


} else if (prior == "weak" & betaprior == "") {
    #Setting up the model string
    Priors<- "for (j in 1:K) {
    Estimate[j] ~ dnorm(0, 1E-6)
    }"

    ivw_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")

} else if (prior == "pseudo" & betaprior == "") {

    #Setting up the model string
  Priors<- "for (j in 1:K) {
  Estimate[j] ~ dt(0, 1, 1)
  }"
  ivw_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")
}

if (betaprior != "") {

  Priors<- paste0("for (j in 1:K) {Estimate[j] ~ ",betaprior,"}")
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
    N = length(object$beta.outcome),
    K = ncol(object$beta.exposure),
    by = object$beta.outcome,
    bx = as.matrix(object$beta.exposure),
    byse = object$se.outcome
  ),
  n.chains = n.chains,
  inits = initsopt,
  quiet = TRUE,
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

nsnps <- length(object$beta.outcome)

mcmciter <- n.iter + n.burn

# Outputs from the model

#Causal Estimate
causal.est <- p$statistics[,1]

#standard deviation
standard.dev <- p$statistics[,2]

#lower Credible Interval for estimates
lower.credible_interval <- p$quantiles[,1]

#Median Interval for estimates
Median_interval <- p$quantiles[,3]

#higher Credible Interval for estimates
Higher.credible_interval <- p$quantiles[,5]

credible_interval <-
  cbind(lower.credible_interval,
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

class(out) <- "mvivwjags"
return(out)

}

#Function for output of results
#' @export
print.mvivwjags <- function(x, ...) {
  outt <-
    matrix(cbind(x$CausalEffect, x$StandardError, x$CredibleInterval),
           nrow = length(x$CausalEffect),
           ncol = 5,
           byrow = F,
           dimnames =list(paste0("Causal Effect",1:length(x$CausalEffect)), c("Estimate", "SD", "2.5%", "50%", "97.5%"))
    )
  print(outt, ...)
  invisible(x)
}

# Generating a summary of the results
#' @export
summary.mvivwjags <- function(object, ...) {
  out <- object
  out1 <-
    matrix(
      cbind(out$CausalEffect, out$StandardError, out$CredibleInterval),
      nrow = length(out$CausalEffect),
      ncol = 5,
      byrow = F,
      dimnames =list(paste0("Causal Effect",1:length(out$CausalEffect)), c("Estimate", "SD", "2.5%", "50%", "97.5%"))
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
