#' Organises the summary level data for use in the Bayesian MR functions
#'
#' @param rsid A vector of genetic variants used for analysis, if unspecified a vector is automatically generated.
#' @param xbeta A matrix of multiple instrument-phenotypes associations.
#' @param ybeta A numeric vector of the instrument-outcome associations.
#' @param xse The matrix for corresponding standard errors of the instrument-phenotypes associations `xbeta`.
#' @param yse The standard errors of the instrument-outcome associations `ybeta`.
#'
#' @export
#' @return A formatted data frame for analysis of class `mvmr_format`.
#'
#' @examples
#' data(dodata)
#' dat <- mvmr_format(rsid = dodata$rsid,
#'           xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
#'           ybeta = dodata$chdbeta,
#'           xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
#'           yse = dodata$chdse)
#' class(dat)
mvmr_format <- function(rsid, xbeta, ybeta, xse, yse) {
  if (missing(rsid)) {
    rsid <- 1:length(ybeta)
    message("SNP id variable generated equal to row number in data frame")
  }

  datm <- list()
  datm$beta.outcome <- ybeta
  datm$beta.exposure <- xbeta
  datm$se.outcome <- yse
  datm$se.exposure <- xse

  class(datm) <- append(class(datm), "mvmr_format")
  return(datm)
}
