# mrbayes

Bayesian implementation of IVW and MR-Egger models.

## Installation instructions

Install the CRAN version with the following code:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"mrbayes"``)`

Or install the development version from r-universe with

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"mrbayes"``, repos ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"https://mrcieu.r-universe.dev"``, ``"https://cloud.r-project.org"``)``)`

or from GitHub with:

\
`# install.packages("remotes") # uncomment if remotes not installed`\
`remotes``::`[`install_github`](https://remotes.r-lib.org/reference/install_github.html)`(``"okezie94/mrbayes"``)`

### Installing JAGS to use the JAGS functions

The functions which use JAGS require that the JAGS software is
installed.

On macOS the easiest way to install JAGS is through Homebrew with

``` sh
brew install pkg-config
brew install jags
```

Alternatively, JAGS installation files for Windows and macOS are
available from
<https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/>, and
further info can be found on the JAGS website
<https://mcmc-jags.sourceforge.io/>.

In R you can then install **rjags** from source

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"rjags"``, type ``=`` ``"source"``)`

## Package website

The helpfiles are shown on the package website at:
<https://okezie94.github.io/mrbayes/>.

## Authors

Okezie Uche-Ikonne, Frank Dondelinger, and Tom Palmer
