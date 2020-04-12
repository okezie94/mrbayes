# mrbayes

[![Build Status](https://travis-ci.org/okezie94/mrbayes.svg?branch=master)](https://travis-ci.org/okezie94/mrbayes)
[![Coverage status](https://codecov.io/gh/okezie94/mrbayes/branch/master/graph/badge.svg)](https://codecov.io/github/okezie94/mrbayes?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/mrbayes)](https://cran.r-project.org/package=mrbayes)
[![RStudio_CRAN_mirror_downloads_badge](http://cranlogs.r-pkg.org/badges/grand-total/mrbayes?color=blue)](https://CRAN.R-project.org/package=mrbayes)

Bayesian implementation of IVW and MR-Egger models.


## Installation instructions
 
Install the CRAN version with following code:
``` r
install.packages("mrbayes")
``` 

Or install the development version from GitHub with:
 
``` r
# install.packages("remotes") # uncomment on first run
remotes::install_github("okezie94/mrbayes", 
                        build_opts = c("--no-resave-data", "--no-manual"), 
                        build_vignettes = TRUE)
```

## Package website

The helpfiles are shown on the package website at: <https://okezie94.github.io/mrbayes/>.

## Authors
Okezie Uche-Ikonne (maintainer, o.uche-ikonne@lancaster.ac.uk), Frank Dondelinger, and Tom Palmer
