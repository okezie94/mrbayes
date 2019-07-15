#' Dataset from Richmond et. al 2017 investigating the association of BMI on insulin resistance
#'
#' A two-sample summary level dataset containing 14 single nucleiodtide polymorphisms which have phenotype-exposure associations (BMI) and phenotype-outcome (insulin) with their respective standard errors.
#'
#' bmi_insulin.
#'
#' @format A data frame with 14 rows and 44 columns:
#' \describe{
#'     \item{rsid}{The genetic variant used as instruments}
#'     \item{beta.exposure}{The value for the phenotype-exposure association}
#'     \item{beta.outcome}{The value for the phenotype-outcome association}
#'     \item{se.exposure}{The value for the standard error of the phenotype-exposure association}
#'     \item{se.outcome}{The value for the standard error of the phenotype-outcome association}
#' }
#'
"bmi_insulin"
