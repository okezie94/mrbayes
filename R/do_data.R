#' Dataset from Do et al., Nat Gen, 2013 containing summary level data on associations of genotypes with lipid traits and the risk of coronary heart diseases
#'
#' A summary-level dataset, from Do et al. (2013) <https://dx.doi.org/10.1038/ng.2795>, containing 185 single nucleiodtide polymorphisms (SNPs) which have genotype-phenotype associations and standard errors for
#' low-density lipoprotein cholestrol, high-density lipoprotein cholestrol and triglycerides, and genotype-outcome associations for (coronary heart disease)
#' with their respective standard errors.
#'
#' do_data.
#'
#' @format A data frame with 185 rows and 9 columns:
#' \describe{
#'       \item{rsid}{SNPs RSID number}
#'       \item{ldlcbeta}{The genotype-phenotype associations for low-density lipoprotein cholestrol}
#'       \item{hdlcbeta}{The genotype-phenotype associations for high-density lipoprotein cholestrol}
#'       \item{tgbeta}{The genotype-phenotype associations for triglycerides}
#'       \item{chdbeta}{The genotype-outcome associations in this case the outcome is coronary heart disease}
#'       \item{ldlcse}{The standard errors of the genotype-low-density lipoprotein cholestrol associations}
#'       \item{hdlcse}{The standard errors of the genotype-high-density lipoprotein cholestrol associations}
#'       \item{tgse}{The standard errors of the genotype-triglyceride associations}
#'       \item{chdse}{The standard errors of the genotype-outcome coronary heart disease associations}
#'  }
#' @references Do, R. et al., Common variants associated with plasma triglycerides and risk for coronary artery disease. Nature Genetics, 2013, 45, 1345-1352, <https://dx.doi.org/10.1038/ng.2795>.
#'
"do_data"
