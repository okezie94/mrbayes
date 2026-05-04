# Dataset from Do et al., Nat Gen, 2013 containing summary level data on associations of genotypes with lipid traits and the risk of coronary heart diseases

A summary-level dataset, from Do et al. (2013)
[doi:10.1038/ng.2795](https://doi.org/10.1038/ng.2795) , containing 185
single nucleotide polymorphisms (SNPs) which have genotype-phenotype
associations and standard errors for LDL-C, HDL-C, Triglycerides, and
genotype-outcome associations for coronary heart disease with their
respective standard errors.

## Usage

``` r
dodata
```

## Format

A data frame with 185 rows and 21 columns with the following variables:

- rsid:

  RSID number

- a1:

  Allele 1

- a2:

  Allele 2

- chr:

  Chromosome

- pos:

  Genomic position

- ldlcbeta:

  The genotype-low-density lipoprotein cholesterol associations

- hdlcbeta:

  The genotype-high-density lipoprotein cholesterol associations

- tgbeta:

  The genotype-triglyceride associations

- chdbeta:

  The genotype-coronary heart disease associations, on the log odds
  ratio scale

- ldlcp2:

  P-value for genotype-LDL-C associations

- hdlcp2:

  P-value for genotype-HDL-C associations

- tgp2:

  P-value for genotype-triglyceride associations

- chdp2:

  P-value for genotype-coronary heart disease associations

- ldlcz:

  Z-score for genotype-LDL-C associations

- ldlcse:

  The standard errors of the genotype-low-density lipoprotein
  cholesterol associations

- hdlcz:

  Z-score for genotype-HDL-C associations

- hdlcse:

  The standard errors of the genotype-high-density lipoprotein
  cholesterol associations

- tgz:

  Z-score for genotype-triglyceride associations

- tgse:

  The standard errors of the genotype-triglyceride cholesterol
  associations

- chdz:

  Z-score for genotype-coronary heart disease associations

- chdse:

  The standard errors of the genotype-coronary heart disease
  associations

## Details

dodata.

## References

Do, R. et al., Common variants associated with plasma triglycerides and
risk for coronary artery disease. Nature Genetics, 2013, 45, 1345-1352,
[doi:10.1038/ng.2795](https://doi.org/10.1038/ng.2795) .
