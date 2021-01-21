#' Dataset from Richmond et. al 2017 investigating the association of BMI on insulin resistance
#'
#' A two-sample summary level dataset, Richmond et al. (2017) \doi{10.1101/155739}, containing 14 single nucleiodtide polymorphisms (SNPs) which have genotype-phenotype associations (BMI) and genotype-outcome associations (insulin) with their respective standard errors.
#'
#' bmi_insulin.
#'
#' @format A data frame with 14 rows and 44 columns:
#' \describe{
#'     \item{rsid}{SNP RSID number}
#'     \item{beta.exposure}{The genotype-BMI associations}
#'     \item{beta.outcome}{The genotype-outcome associations}
#'     \item{se.exposure}{The standard errors of the genotype-phenotype associations}
#'     \item{se.outcome}{The standard errors of the genotype-outcome associations}
#' }
#' @references Richmond, R. et al., Investigating the role of insulin in increased adiposity: Bi-directional Mendelian randomization study. bioRxiv, 2017, \doi{10.1101/155739}.
#'
"bmi_insulin"
