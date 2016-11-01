# quickmatch

[![Build Status](https://travis-ci.org/fsavje/quickmatch.svg?branch=master)](https://travis-ci.org/fsavje/quickmatch)
[![Build status](https://ci.appveyor.com/api/projects/status/beypek5qq868d4yf/branch/master?svg=true)](https://ci.appveyor.com/project/fsavje/quickmatch/branch/master)
[![codecov](https://codecov.io/gh/fsavje/quickmatch/branch/master/graph/badge.svg)](https://codecov.io/gh/fsavje/quickmatch)

R package for matching in large data sets with complicated constraints

## Install quickmatch

`quickmatch` is currently under development and is not yet available on CRAN. The development version and its dependencies can be installed with [devtools](https://github.com/hadley/devtools) using the following:
```R
# Install devtools, if not already installed, with command:
# install.packages("devtools")

# Install dependencies
devtools::install_github("fsavje/Rscclust")

# Install quickmatch
devtools::install_github("fsavje/quickmatch")
```