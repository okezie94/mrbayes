#' Dataset from Do et al., Nat Gen, 2013 containing summary level data on associations of genotypes with lipid traits and the risk of coronary heart diseases
#'
#' A summary-level dataset, from Do et al. (2013) \doi{10.1038/ng.2795},
#' containing 185 single nucleodtide polymorphisms (SNPs) which have genotype-phenotype associations and standard errors for
#' LDL-C, HDL-C, Triglycerides, and genotype-outcome associations for coronary heart disease
#' with their respective standard errors.
#'
#' dodata.
#'
#' @format A data frame with 185 rows and 21 columns with the relevant features:
#' \describe{
#'       \item{rsid}{SNPs RSID number}
#'       \item{a1}{}
#'       \item{a2}{}
#'       \item{chr}{}
#'       \item{pos}{}
#'       \item{ldlcbeta}{The genotype-phenotype associations for low-density lipoprotein cholestrol}
#'       \item{hdlcbeta}{The genotype-phenotype associations for high-density lipoprotein cholestrol}
#'       \item{tgbeta}{The genotype-phenotype associations for Triglyceride}
#'       \item{chdbeta}{The genotype-outcome associations in this case the outcome is coronary heart disease}
#'       \item{ldlcp2}{P-value for genotype-phenotype associations of LDL-C}
#'       \item{hdlcp2}{P-value for genotype-phenotype associations of HDL-C}
#'       \item{tgp2}{P-value for genotype-phenotype associations of Triglycerides}
#'       \item{chdp2}{P-value for genotype-phenotype associations of coronary heart disease}
#'       \item{ldlcz}{Z-score for genotype-phenotype associations of LDL-C}
#'       \item{ldlcse}{The standard errors of the genotype-low-density lipoprotein cholestrol associations}
#'       \item{hdlcz}{Z-score for genotype-phenotype associations of HDL-C}
#'       \item{hdlcse}{The standard errors of the genotype-high-density lipoprotein cholestrol associations}
#'       \item{tgz}{Z-score for genotype-phenotype associations of triglyceride}
#'       \item{tgse}{The standard errors of the genotype-triglyceride cholestrol associations}
#'       \item{chdz}{Z-score for genotype-phenotype associations of coronary heart disease}
#'       \item{chdse}{The standard errors of the genotype-outcome coronary heart disease associations}
#'
#'  }
#' @references Do, R. et al., Common variants associated with plasma triglycerides and risk for coronary artery disease. Nature Genetics, 2013, 45, 1345-1352, \doi{10.1038/ng.2795}.
#'
"dodata"
