#' Organises the summary level data for use in the Bayesian MR functions
#'
#' @param rsid A vector of number of genetic variants used for analysis, if none is given a vector would automatically generate.
#' @param xbeta A numeric vector of the coefficients of the genotype-exposure associations.
#' @param ybeta A numeric vector of the coefficients of the genotype-outcome associations.
#' @param xse The standard errors correspondent to the genotype-exposure associations `xbeta`.
#' @param yse The standard errors correspondent to the genotype-outcome associations `ybeta`.
#'
#' @export
#' @return A formatted data frame for analysis.
#'
#' @examples
#'
#' data(bmi_insulin)
#' dat <- mr_format(rsid = bmi_insulin[,"rsid"],
#'           xbeta = bmi_insulin[,"beta.exposure"],
#'           ybeta = bmi_insulin[,"beta.outcome"],
#'           xse = bmi_insulin[,"se.exposure"],
#'           yse = bmi_insulin[,"se.outcome"])
#' class(dat)
#'
#Function for formatting
mr_format <- function(rsid, xbeta, ybeta, xse, yse) {
  if (missing(rsid)) {
    rsid <- 1:length(ybeta)
    message("SNP id variable generated equal to row number in data frame")
  }

  datm <- data.frame(rsid, xbeta, ybeta, xse, yse)

  names(datm) <-
    c("rsid",
      "beta.exposure",
      "beta.outcome",
      "se.exposure",
      "se.outcome")

  class(datm) <- append(class(datm), "mr_format")
  return(datm)

}
