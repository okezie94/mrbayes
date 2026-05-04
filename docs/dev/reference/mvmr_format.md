# Organises the summary level data for use in the Bayesian MR functions

Organises the summary level data for use in the Bayesian MR functions

## Usage

``` r
mvmr_format(rsid, xbeta, ybeta, xse, yse)
```

## Arguments

- rsid:

  A vector of genetic variants used for analysis, if unspecified a
  vector is automatically generated.

- xbeta:

  A matrix of multiple instrument-phenotypes associations.

- ybeta:

  A numeric vector of the instrument-outcome associations.

- xse:

  The matrix for corresponding standard errors of the
  instrument-phenotypes associations `xbeta`.

- yse:

  The standard errors of the instrument-outcome associations `ybeta`.

## Value

A formatted data frame for analysis of class `mvmr_format`.

## Examples

``` r
data(dodata)
dat <- mvmr_format(
  rsid = dodata$rsid,
  xbeta = cbind(dodata$ldlcbeta,dodata$hdlcbeta,dodata$tgbeta),
  ybeta = dodata$chdbeta,
  xse = cbind(dodata$ldlcse,dodata$hdlcse,dodata$tgse),
  yse = dodata$chdse
)
class(dat)
#> [1] "list"        "mvmr_format"
```
