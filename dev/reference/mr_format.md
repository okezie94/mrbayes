# Organises the summary level data for use in the Bayesian MR functions

Organises the summary level data for use in the Bayesian MR functions

## Usage

``` r
mr_format(rsid, xbeta, ybeta, xse, yse)
```

## Arguments

- rsid:

  A vector of genetic variants used for analysis, if unspecified a
  vector is automatically generated.

- xbeta:

  A numeric vector of the instrument-phenotype associations.

- ybeta:

  A numeric vector of the instrument-outcome associations.

- xse:

  The standard errors of the instrument-phenotype associations `xbeta`.

- yse:

  The standard errors of the instrument-outcome associations `ybeta`.

## Value

A formatted data frame for analysis of class `mr_format`.

## Examples

``` r
data(bmi_insulin)
dat <- mr_format(
  rsid = bmi_insulin[,"rsid"],
  xbeta = bmi_insulin[,"beta.exposure"],
  ybeta = bmi_insulin[,"beta.outcome"],
  xse = bmi_insulin[,"se.exposure"],
  yse = bmi_insulin[,"se.outcome"]
)
class(dat)
#> [1] "data.frame" "mr_format" 
```
