#' Bayesian implementation of the MVMR-Egger model with choice of prior distributions fitted using JAGS.
#'
#' Bayesian implementation of the MVMR-Egger model with choice of prior distributions fitted using JAGS.
#'
#' @param object A data object of class [`mvmr_format`].
#' @param prior A character string for selecting the prior distributions;
#'
#' * `"default"` selects a non-informative set of priors;
#' * `"weak"` selects weakly informative priors;
#' * `"pseudo"` selects a pseudo-horseshoe prior on the causal effect;
#' @param betaprior A character string in JAGS syntax to allow a user defined prior for the causal effect.
#' @param sigmaprior A character string in JAGS syntax to allow a user defined prior for the residual standard deviation.
#' @param n.chains Numeric indicating the number of chains used in the MCMC estimation, the default is `3` chains.
#' @param n.burn Numeric indicating the burn-in period of the Bayesian MCMC estimation. The default is `1000` samples.
#' @param n.iter Numeric indicating the number of iterations in the Bayesian MCMC estimation. The default is `5000` iterations.
#' @param seed Numeric indicating the random number seed. The default is the rjags default.
#' @param rho Numeric indicating the correlation coefficient input into the joint prior distribution. The default value is `0.5`.
#' @param orientate Numeric value to indicate the oriented exposure
#' @param ... Additional arguments passed through to [`rjags::jags.model()`].
#'
#' @export
#' @return An object of class `mveggerjags` containing the following components:
#' \describe{
#' \item{AvgPleio}{The mean of the simulated pleiotropic effect}
#' \item{CausalEffect}{The mean of the simulated causal effect}
#' \item{StandardError}{Standard deviation of the simulated causal effect}
#' \item{sigma}{The value of the residual standard deviation}
#' \item{CredibleInterval}{The credible interval for the causal effect, which includes the lower (2.5%), median (50%) and upper intervals (97.5%)}
#' \item{samples}{Output of the Bayesian MCMC samples}
#' \item{Priors}{The specified priors}
#' }
#'
#' @references Bowden et. al., Mendelian randomization with invalid instruments: effect estimation and bias detection through Egger regression. International Journal of Epidemiology 2015. 44(2): p. 512-525. \doi{10.1093/ije/dyv080}
#' @examples
#' if (requireNamespace("rjags", quietly = TRUE)){
#' \dontrun{
#' dat <- mvmr_format(
#'   rsid = dodata$rsid,
#'   xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
#'   ybeta = dodata$chdbeta,
#'   xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
#'   yse = dodata$chdse
#' )
#'
#' fit <- mvmr_egger_rjags(dat)
#' summary(fit)
#' plot(fit$samples)
#' # 90% credible interval
#' fitdf <- do.call(rbind.data.frame, fit$samples)
#' cri90 <- sapply(fitdf, quantile, probs = c(0.05, 0.95))
#' print(cri90)
#' }
#' }
mvmr_egger_rjags <- function(object,
                             prior = "default",
                             betaprior = "",
                             sigmaprior = "",
                             orientate = 1,
                             n.chains = 3,
                             n.burn = 1000,
                             n.iter = 5000,
                             seed = NULL,
                             rho = 0.5,
                             ...) {

  # convert MRInput object to mvmr_format
  # if ("MVMRInput" %in% class(object)) {
  #   object <- mrinput_mvmr_format(object)
  # }

  # check class of object
  if (!("mvmr_format" %in% class(object))) {
    stop('The class of the data object must be "mvmr_format", please resave the object with the output of e.g. object <- mvmr_format(object).')
  }

  # check if rjags is installed
  rjags_check()

  # orientation setup

  if (orientate %in% seq_len(dim(object$beta.exposure)[2])) {
    orientAte <- orientate
  } else {
    orientAte <- 1
  }

  orient <- sign(object$beta.exposure)[, orientAte]

  # String for likelihood

  Likelihood <-
    "for (i in 1:N){
    by[i] ~ dnorm(by.hat[i], tau[i])
    by.hat[i] <- Pleiotropy + inprod(Estimate[], bx[i,])
    tau[i] <- pow(byse[i] * sigma, -2)
    }"


  # non-informative prior

  if (prior == "default" && betaprior == "") {

    #Setting up the model string
    Priors <- "Pleiotropy ~ dnorm(0, 1E-3) \n
    for (j in 1:K) {
    Estimate[j] ~ dnorm(0,1E-3)
    } \n sigma ~ dunif(.0001, 10)"

    egger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")

    # weakly informative prior

  } else if (prior == "weak" && betaprior == "") {

    # Setting up the model string
    Priors <- "Pleiotropy ~ dnorm(0, 1E-6) \n
    for (j in 1:K) {
    Estimate[j] ~ dnorm(0,1E-6)
    } \n
    sigma ~ dunif(.0001, 10)"
    egger_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")


    # pseudo-shrinkage prior
  } else if (prior == "pseudo" && betaprior == "") {
    #Setting up the model string
    Priors <- "Pleiotropy ~ dnorm(0,1E-3) \n
    for (j in 1:K) {
    Estimate[j] ~ dt(0, 1, 1)} \n
    invpsi ~ dgamma(1E-3, 1E-3)\n
    sigma <- 1/invpsi"
    egger_model_string <- paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")

    # joint prior
  } else if (prior == "joint" && betaprior == "") {
    #setting up model string

    # covariance matrix
    vcov_mat <- "
    beta[1:l] ~ dmnorm.vcov(mu[], prec[ , ])\n
    Pleiotropy <- beta[1]
    for (i in 2:K){
      Estimate[i] <- beta[i]
    }
    for (i in 1:l){
     for (j in 1:l){
     mu[i] <- 0
     var[i] <- 1e4
     sd <- sqrt(var)
     prec[i,j] <- sd[i] * sd[j] * rho
     prec[j,i] <- sd[j] * sd[i] * rho
     prec[i,i] <- var[i]
     }
    }
    sigma ~ dunif(.0001, 10)
    rho <-"

    # vcov_mat<- "
    # beta[1:2] ~ dmnorm.vcov(mu[], prec[ , ])\n
    # Pleiotropy <- beta[1]
    # for (j in 1:K) {
    # Estimate[j] ~ dnorm(0,1E-3)
    # }
    # prec[1,1] <- var1
    # prec[1,2] <- sd1*sd2*rho
    # prec[2,1] <- sd1*sd2*rho
    # prec[2,2] <- var2
    # mu[1] <- 0
    # mu[2] <- 0
    # var1 <- 1e4
    # sd1 <- sqrt(var1)
    # var2 <- 1e4
    # sd2<- sqrt(var2)
    # sigma ~ dunif(.0001, 10)
    # rho <- "

    Priors <- paste0(vcov_mat, rho)

    #Priors <- "Pleiotropy ~ dnorm(0, 1E-6) \n Estimate ~ dnorm(0, 1E-6) \n sigma ~ dunif(.0001, 10)"

    egger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")

  } else if (betaprior != "" && sigmaprior != "") {
    part1 <- "Pleiotropy ~ dnorm(0, 1E-3) \n for (j in 1:K) {Estimate[j] ~ "
    part2 <- "\n sigma ~ "
    Priors <- paste0(part1, betaprior, "}", part2, sigmaprior)

    egger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n }")

  } else if (betaprior != "" && sigmaprior == "") {
    part1 <- "Pleiotropy ~ dnorm(0, 1E-3) \n for (j in 1:K) {Estimate[j] ~ "
    part2 <- "\n sigma ~ dunif(.0001,10)"
    Priors <- paste0(part1, betaprior, "}", part2)

    egger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n }")

  } else if (betaprior == "" && sigmaprior != "") {
    part1 <- "Pleiotropy ~ dnorm(0, 1E-3) \n for (j in 1:K) {
    Estimate[j] ~ dnorm(0, 1E-6)} \n sigma ~"
    Priors <- paste0(part1, sigmaprior)

    egger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n }")

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

  egger_model <- rjags::jags.model(
    textConnection(egger_model_string),
    data = list(
      N = length(object$beta.outcome),
      K =  ncol(object$beta.exposure),
      by = orient * object$beta.outcome,
      bx = orient * object$beta.exposure,
      byse = object$se.outcome
    ),
    n.chains = n.chains,
    inits = initsopt,
    quiet = TRUE,
    ...
  )
  # Burn-in
  update.jags <- utils::getFromNamespace("update.jags", "rjags")
  update.jags(egger_model, n.iter = n.burn)

  # Collect samples

  egger_samp <- rjags::coda.samples(
    egger_model,
    variable.names = c("Pleiotropy", "Estimate", "sigma"),
    n.iter = n.iter
  )

  # eggersamp2 <- rjags::coda.samples(
  #   egger_model,
  #   variable.names = c("beta", "sigma"),
  #   n.iter = n.iter
  # )

  #egger_samp <- if (prior != "joint" & betaprior == ""){eggersamp1} else {eggersamp2}

  g <- egger_samp

  p <- summary(egger_samp)

  prior <- prior

  niter <- n.iter

  nburn <- n.burn

  nchain <- n.chains

  nsnps <- length(object$beta.outcome)

  mcmciter <- n.iter + n.burn

  # Outputs from the model

  #Average Pleiotropic effect
  avg.pleio <- p$statistics[ncol(object$beta.exposure) + 1, 1]

  #Standard dev for AVg Pleio
  avg.pleiostd <- p$statistics[ncol(object$beta.exposure) + 1, 2]

  #lower credible interval
  avg.pleioLI <- p$quantiles[ncol(object$beta.exposure) + 1, 1]

  #mdeian credible interval
  avg.pleioM <- p$quantiles[ncol(object$beta.exposure) + 1, 3]

  #Upper credible interval
  avg.pleioUI <- p$quantiles[ncol(object$beta.exposure) + 1, 5]

  CI_avgpleio <- c(avg.pleioLI, avg.pleioM, avg.pleioUI)

  #Inflating Parameter
  sigma <- p$statistics[ncol(object$beta.exposure) + 2, 1]
  #Causal Estimate
  causal.est <- p$statistics[seq_len(ncol(object$beta.exposure)), 1]

  #standard deviation
  standard.dev <- p$statistics[seq_len(ncol(object$beta.exposure)), 2]

  #lower Credible Interval for estimates
  lower.credible_interval <- p$quantiles[seq_len(ncol(object$beta.exposure)), 1]

  #Median Interval for estimates
  Median_interval <- p$quantiles[seq_len(ncol(object$beta.exposure)), 3]

  #higher Credible Interval for estimates
  Higher.credible_interval <- p$quantiles[seq_len(ncol(object$beta.exposure)), 5]

  credible_interval <-
    c(lower.credible_interval,
      Median_interval,
      Higher.credible_interval)

  # warning for residual error less than 1

  if (sigma < 1) {
    warning("The mean of the sigma parameter, the residual standard deviation, we recommend refitting the model with sigma constrained to be >= 1.")
    # sigma ~ #### T(1,) # ;T(1,)
  }

  #Class for the output
  out <- list()
  out$CausalEffect <- causal.est
  out$StandardError <- standard.dev
  out$lower.credible_interval <- lower.credible_interval
  out$Median_interval <- Median_interval
  out$Higher.credible_interval <- Higher.credible_interval
  out$CredibleInterval <- credible_interval
  out$AvgPleio <- avg.pleio
  out$AvgPleioSD <- avg.pleiostd
  out$AvgPleioCI <- CI_avgpleio
  out$sigma <- sigma
  out$samples <- g
  out$priormethod <- prior
  out$betaprior <- betaprior
  out$sigmaprior <- sigmaprior
  out$samplesize <- niter
  out$burnin <- nburn
  out$chains <- nchain
  out$MCMC <- mcmciter
  out$nsnps <- nsnps
  out$Prior <- Priors
  out$model <- egger_model_string

  class(out) <- "mveggerjags"
  return(out)

}

#Function for output of results
#' @export
print.mveggerjags <- function(x, ...) {
  estmat <- matrix(ncol = 5, nrow = length(x$CausalEffect))
  for (i in 1:3){
    estmat[i, ] <- c(x$CausalEffect[i], x$StandardError[i], x$lower.credible_interval[i],
                     x$Median_interval[i], x$Higher.credible_interval[i])
  }
  pleiomat <- c(x$AvgPleio, x$AvgPleioSD, x$AvgPleioCI)
  outt <-
    matrix(
      rbind(pleiomat,estmat),
      nrow = 1 + length(x$CausalEffect),
      ncol = 5,
      dimnames = list(
        c("Avg Pleio", paste0("Causal Effect", seq_along(x$CausalEffect))),
        c("Estimate", "SD", "2.5%", "50%", "97.5%")
      )
    )
  print(outt)
  invisible(x)
}

# Generating a summary of the results
#' @export
summary.mveggerjags <- function(object, ...) {
  out <- object
  estmat <- matrix(ncol = 5, nrow = length(out$CausalEffect))
  for (i in 1:3){
    estmat[i, ] <- c(out$CausalEffect[i], out$StandardError[i], out$lower.credible_interval[i],
                     out$Median_interval[i], out$Higher.credible_interval[i])
  }
  pleiomat <- c(out$AvgPleio, out$AvgPleioSD, out$AvgPleioCI)
  out1 <-
    matrix(
      rbind(pleiomat, estmat),
      nrow = 1 + length(out$CausalEffect),
      ncol = 5,
      dimnames = list(
        c("Avg Pleio", paste0("Causal Effect", seq_along(out$CausalEffect))),
        c("Estimate", "SD", "2.5%", "50%", "97.5%")
      )
    )

  #Generate statements for output

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

  cat("Inflating Parameter:", out$sigma, "\n\n")

  print(out1, ...)

}
