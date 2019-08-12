#' Fitting Bayesian MR-radialegger models using various priors from the jags software.
#'
#' @param object The data frame converted into the format_mr format
#' @param prior The option for selecting the proposed priors; "default" indicates non-informative prior; "weak" indicates weakly informative prior; "pseudo" indicates pseudo-horseshoe prior; "joint" indicates joint probability prior.
#' @param betaprior This is an option for setting a prior for the causal estimate.
#' @param sigmaprior This is an option for setting a prior for the inflating parameter in the Radial formulation of MR-Egger model.
#' @param n.chains This is an option for choosing the number of chains for MCMC simulation, default number is 3 chains.
#' @param n.burn This is the option for the burn in period of the bayesian MCMC runs. The default option is 1000 samples
#' @param n.iter This is the option for the number of bayesian MCMC runs. The default is 5000 iterations
#' @param seed This is an option for setting seeds for reproducible results. The default is NULL
#' @param rho The correlation coefficient factor used to analyze the joint prior method
#' @param ... Passing options through to rjags::jags.model()
#'
#' @export
#' @return The result object of class radialeggerjags contains the following components:
#' \describe{
#' \item{AvgPleio}{The mean of the generated pleiotropic effect}
#' \item{CausalEffect}{The mean of the generated causal effects}
#' \item{StandardError}{Standard deviation of the mean causal effect}
#' \item{psi}{The value of the inflating parameter based on the priors}
#' \item{CredibleInterval}{The credible interval for the causal effect, which indicates the lower(2.5\%), median (50\%) and upper intervals (97.5\%)}
#' \item{samples}{Output of the bayesian MCMC samples with the different chains}
#' \item{Priors}{The specified priors}
#' }
#'
#' @references Bowden, J., et al., Improving the visualization, interpretation and analysis of two-sample summary data Mendelian randomization via the Radial plot and Radial regression. International Journal of Epidemiology, 2018. 47(4): p. 1264-1278. <https://doi.org/10.1093/ije/dyy101>
#' @examples
#' data(bmi_insulin)
#' fit <- mr_radialegger_rjags(bmi_insulin, n.chains = 1)
#' summary(fit)
#' plot(fit$samples)
#'
mr_radialegger_rjags <- function(object,
                                 prior = "default",
                                 betaprior = "",
                                 sigmaprior = "",
                                 n.chains = 1,
                                 n.burn = 1000,
                                 n.iter = 5000,
                                 seed = NULL,
                                 rho = 0.5,
                                 ...) {

  # check class of object
  if (!("mr_format" %in% class(object))) {
    stop(warning('The class of the data object must be "mr_format", please resave the object with the output of e.g. object <- mr_format(object).'))
  }

  # Strings for likelihood

  Likelihood <-
    "for (i in 1:N){
    by[i] ~ dnorm(by.hat[i], psi)
    by.hat[i] <- Pleiotropy + Estimate * bx[i]
    }"

  # Conditional statements for the prior statements

  if (prior == "default" & betaprior == "") {
    #Setting up the model string
    Priors <-"Pleiotropy ~ dnorm(0, 1E-3) \n Estimate ~ dnorm(0, 1E-3) \n psi ~ dunif(.0001, 10)"

    radialegger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")

  } else if (prior == "weak" & betaprior == "") {
    #Setting up the model string

    Priors <- "Pleiotropy ~ dnorm(0, 1E-6) \n Estimate ~ dnorm(0, 1E-6) \n psi ~ dunif(.0001, 10)"


    radialegger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")

  } else if (prior == "pseudo" & betaprior == "") {
    #Setting up the model string
    Priors <-"Pleiotropy ~ dnorm(0,1E-3) \n Estimate ~ dt(0, 1, 1) \n invpsi ~ dgamma(1E-3, 1E-3) \n psi <- 1/invpsi"

    radialegger_model_string <-
      paste0("model {", Likelihood, "\n\n", Priors, "\n\n}")
  }

  else if (prior == "joint" & betaprior == ""){
    #setting up model string

    # covariance matrix
    vcov_mat<- "
    beta[1:2] ~ dmnorm.vcov(mu[], prec[ , ])
    Pleiotropy <- beta[1]
    Estimate <- beta[2]
    prec[1,1] <- var1
    prec[1,2] <- sd1*sd2*rho
    prec[2,1] <- sd1*sd2*rho
    prec[2,2] <- var2
    mu[1] <- 0
    mu[2] <- 0
    var1 <- 1e4
    sd1 <- sqrt(var1)
    var2 <- 1e4
    sd2<- sqrt(var2)
    psi ~ dunif(.0001, 10)
    rho <- "

    Priors<- paste0(vcov_mat,rho)

    #Priors <- "Pleiotropy ~ dnorm(0, 1E-6) \n Estimate ~ dnorm(0, 1E-6) \n psi ~ dunif(.0001, 10)"

    radialegger_model_string <-
      paste0("model {", Likelihood,"\n\n",Priors,"\n\n}")

  }

  else if (betaprior != ""  & sigmaprior != "") {
    part1 <-"Pleiotropy ~ dnorm(0, 1E-3) \n Estimate ~ "
    part2 <- "\n psi ~ "
    Priors <- paste0(part1,betaprior,part2,sigmaprior)

    radialegger_model_string <-
      paste0("model {",Likelihood,"\n\n", Priors,"\n\n }")
  } else if (betaprior != ""  & sigmaprior == "") {
    part1 <-"Pleiotropy ~ dnorm(0, 1E-3) \n Estimate ~ "
    part2 <- "\n psi ~ dunif(.0001,10)"
    Priors <- paste0(part1,betaprior,part2)

    radialegger_model_string <-
      paste0("model {",Likelihood,"\n\n", Priors,"\n\n }")

  } else if (betaprior == ""  & sigmaprior != "") {
    part1 <-"Pleiotropy ~ dnorm(0, 1E-3) \n Estimate ~ dnorm(0, 1E-6) \n psi ~"
    Priors <- paste0(part1,sigmaprior)

    radialegger_model_string <-
      paste0("model {",Likelihood,"\n\n", Priors,"\n\n }")
  }


  # ratio estimates and weights

  bj <- object[, 3] / object[, 5]
  wj <- object[, 2] / object[, 5]


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

  radialegger_model <- rjags::jags.model(
    textConnection(radialegger_model_string),
    data = list(
      N = nrow(object),
      by = bj,
      bx = wj
    ),
    n.chains = n.chains,
    inits = initsopt,
    ...
  )
  # Burn-in
  update.jags <- utils::getFromNamespace("update.jags", "rjags")
  update.jags(radialegger_model, n.iter = n.burn)

  # Collect samples
  radialegger_samp <- rjags::coda.samples(
    radialegger_model,
    variable.names = c("Pleiotropy", "Estimate", "psi"),
    n.iter = n.iter
  )

  g <- radialegger_samp

  p <- summary(radialegger_samp)

  prior <- prior

  niter <- n.iter

  nburn <- n.burn

  nchain <- n.chains

  nsnps <- nrow(object)

  mcmciter <- n.iter + n.burn

  # Outputs from the model

  #Average Pleiotropic effect
  avg.pleio <- p$statistics[2, 1]

  #Standard dev for AVg Pleio
  avg.pleiostd <- p$statistics[2, 2]

  #lower credible interval
  avg.pleioLI <- p$quantiles[2, 1]

  #mdeian credible interval
  avg.pleioM <- p$quantiles[2, 3]

  #Upper credible interval
  avg.pleioUI <- p$quantiles[2, 5]

  CI_avgpleio <- c(avg.pleioLI, avg.pleioM, avg.pleioUI)

  #Inflating Parameter
  psi <- p$statistics[3, 1]
  #Causal Estimate
  causal.est <- p$statistics[1, 1]

  #standard deviation
  standard.dev <- p$statistics[1, 2]

  #lower Credible Interval for estimates
  lower.credible_interval <- p$quantiles[1, 1]

  #Median Interval for estimates
  Median_interval <- p$quantiles[1, 3]

  #higher Credible Interval for estimates
  Higher.credible_interval <- p$quantiles[1, 5]

  credible_interval <-
    c(lower.credible_interval,
      Median_interval,
      Higher.credible_interval)

  # warning for residual error less than 1

  if (psi < 1) {
    warning("The mean of the psi parameter, the residual standard deviation, is less than 1, we recommend refitting the model with psi constrained to be >= 1.")
  }

  #Class for the output
  out <- list()
  out$CausalEffect <- causal.est
  out$StandardError <- standard.dev
  out$CredibleInterval <- credible_interval
  out$AvgPleio <- avg.pleio
  out$AvgPleioSD <- avg.pleiostd
  out$AvgPleioCI <- CI_avgpleio
  out$psi <- psi
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
  out$model <- radialegger_model_string

  class(out) <- "radialeggerjags"
  return(out)

}

#Function for output of results
#' @export
print.radialeggerjags <- function(x, ...) {
  outt <-
    matrix(
      c(
        x$AvgPleio,
        x$AvgPleioSD,
        x$AvgPleioCI,
        x$CausalEffect,
        x$StandardError,
        x$CredibleInterval
      ),
      nrow = 2,
      ncol = 5,
      byrow = TRUE,
      dimnames = list(
        c("Avg Pleio", "Causal Effect"),
        c("Estimate", "SD", "2.5%", "50%", "97.5%")
      )
    )
  print(outt)
  invisible(x)
}

# Generating a summary of the results
#' @export
summary.radialeggerjags <- function(object, ...) {
  out <- object
  out1 <-
    matrix(
      c(
        out$AvgPleio,
        out$AvgPleioSD,
        out$AvgPleioCI,
        out$CausalEffect,
        out$StandardError,
        out$CredibleInterval
      ),
      nrow = 2,
      ncol = 5,
      byrow = TRUE,
      dimnames = list(
        c("Avg Pleio", "Causal Effect"),
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


  cat("Inflating Parameter:", out$psi, "\n\n")


  print(out1, ...)

}
