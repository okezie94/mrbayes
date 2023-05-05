#' mrbayes: Bayesian implementation of the IVW and MR-Egger models for two-sample Mendelian randomization analyses
#'
#' Bayesian implementation of the IVW and MR-Egger models and their radial and
#' multivariate versions for two-sample Mendelian randomization analyses.
#'
#' @name mrbayes-package
#' @docType package
#' @aliases mrbayes
#' @useDynLib mrbayes, .registration = TRUE
#' @import methods
#' @import Rcpp
#'
#' @references
#' Stan Development Team (2019). RStan: the R interface to Stan. R package version 2.19.2. \url{https://mc-stan.org}
NULL

# To suppress R CMD check NOTE about {package} not imported from
# https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports
ignore_unused_imports <- function() {
  rstantools::use_rstan
  RcppParallel::CxxFlags
}
