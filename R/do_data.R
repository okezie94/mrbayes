#' Dataset from Do et al., Nat Gen, 2013 containing summary level data on associations of genotypes with lipid traits and the risk of coronary heart diseases
#'
#' A summary-level dataset containing 185 single nucleiodtide polymorphisms which have genotype-exposure associations and standard errors for
#' low-density lipoprotein cholestrol, high-density lipoprotein cholestrol and triglyceride exposures. The dataset also contains genotype-outcome (coronary heart disease) associations
#' and standard errors.
#'
#' do_data.
#'
#' @format A data frame with 185 rows and 9 columns:
#' \describe{
#'       \item{rsid}{Identifiers of the genetic variants}
#'       \item{ldlcbeta}{The phenotype-exposure association for low-density lipoprotein cholestrol}
#'       \item{hdlcbeta}{The phenotype-exposure association for high-density lipoprotein cholestrol}
#'       \item{tgbeta}{The phenotype-exposure association for triglycerides}
#'       \item{chdbeta}{The phenotype-outcome association in this case is coronary heart disease}
#'       \item{ldlcse}{The standard error for the exposure low-density lipoprotein cholestrol}
#'       \item{hdlcse}{The standard error for the exposure high-density lipoprotein cholestrol}
#'       \item{tgse}{The standard error for the exposure triglycerides}
#'       \item{chdse}{The standard error for the outcome coronary heart disease}
#'  }
#'
"do_data"
