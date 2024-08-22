# mrbayes

<!-- badges: start -->
[![R-CMD-check](https://github.com/okezie94/mrbayes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/okezie94/mrbayes/actions/workflows/R-CMD-check.yaml)
[![Coverage status](https://codecov.io/gh/okezie94/mrbayes/branch/master/graph/badge.svg)](https://app.codecov.io/github/okezie94/mrbayes?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/mrbayes)](https://cran.r-project.org/package=mrbayes)
[![RStudio_CRAN_mirror_downloads_badge](https://cranlogs.r-pkg.org/badges/grand-total/mrbayes?color=blue)](https://CRAN.R-project.org/package=mrbayes)
<!-- badges: end -->

Bayesian implementation of IVW and MR-Egger models.


## Installation instructions
 
Install the CRAN version with following code:

``` r
install.packages("mrbayes")
``` 

Or install the development version from r-universe with

```r
install.packages("mrbayes", repos = c("https://mrcieu.r-universe.dev", "https://cloud.r-project.org"))
```

or from GitHub with:
 
``` r
# install.packages("remotes") # uncomment if remotes not installed
remotes::install_github("okezie94/mrbayes")
```

### Installing JAGS to use the JAGs functions

Using the functions which use JAGS require that the JAGS software is installed.

On macOS the easiest way to install JAGS is through Homebrew with

```sh
brew install pkg-config
brew install jags
```

Alternatively, JAGS installation files for Windows and macOS are available from <https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/>, and further info can be found on the JAGS website <https://mcmc-jags.sourceforge.io/>.

In R you can then install **rjags** from source

```r
install.packages("rjags", type = "source")
```

## Package website

The helpfiles are shown on the package website at: <https://okezie94.github.io/mrbayes/>.

## Authors

Okezie Uche-Ikonne, Frank Dondelinger, and Tom Palmer
