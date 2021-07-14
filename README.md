# mrbayes

[![Build Status](https://github.com/okezie94/mrbayes/workflows/R-CMD-check/badge.svg)](https://github.com/okezie94/mrbayes/actions?workflow=R-CMD-check)
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
# install.packages("remotes") # uncomment if remotes not installed
remotes::install_github("okezie94/mrbayes")
```
<!--
## Docker container instructions

* Install and launch Docker desktop
* To build the Docker container image run from your terminal
```
bash dockerbuild.sh
```
* To run the Docker container run from your terminal
```
bash dockerrun.sh
```
* Then in a brower go to `http://localhost:8787/`
  * username: rstudio
  * password: pass
* Once in RStudio in the Files pane navigate to the mrbayes folder and click the `.Rproj` file to open the repo as a project
* To compile the binary files run
```r
pkgbuild::compile_dll(force = TRUE)
```
* Edit the files and recompile the binary files as needed
* To stop the container get its id with
```
docker ps
```
* And then run
```
docker stop ####
```
-->

## Package website

The helpfiles are shown on the package website at: <https://okezie94.github.io/mrbayes/>.

## Authors
Okezie Uche-Ikonne (maintainer, okezieikonne@gmail.com), Frank Dondelinger, and Tom Palmer
