#' Organises the summary level data for use in the Bayesian MR functions
#'
#' @param rsid A vector of genetic variants used for analysis, if unspecified a vector is automatically generated.
#' @param xbeta A numeric vector of the instrument-phenotype associations.
#' @param ybeta A numeric vector of the instrument-outcome associations.
#' @param xse The standard errors of the instrument-phenotype associations `xbeta`.
#' @param yse The standard errors of the instrument-outcome associations `ybeta`.
#'
#' @export
#' @return A formatted data frame for analysis of class `mr_format`.
#'
#' @examples
#' data(bmi_insulin)
#' dat <- mr_format(rsid = bmi_insulin[,"rsid"],
#'           xbeta = bmi_insulin[,"beta.exposure"],
#'           ybeta = bmi_insulin[,"beta.outcome"],
#'           xse = bmi_insulin[,"se.exposure"],
#'           yse = bmi_insulin[,"se.outcome"])
#' class(dat)
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
