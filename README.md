# quickmatch

[![Build Status](https://travis-ci.org/fsavje/quickmatch.svg?branch=master)](https://travis-ci.org/fsavje/quickmatch)
[![Build status](https://ci.appveyor.com/api/projects/status/beypek5qq868d4yf/branch/master?svg=true)](https://ci.appveyor.com/project/fsavje/quickmatch/branch/master)
[![codecov](https://codecov.io/gh/fsavje/quickmatch/branch/master/graph/badge.svg)](https://codecov.io/gh/fsavje/quickmatch)

`quickmatch` provides functions for constructing near-optimal generalized full
matchings. Generalized full matching is an extension of the original full matching
method to situations with more intricate study designs. The package is made with
large data sets in mind and derives matchings more than an order of magnitude
quicker than other methods.


## Install quickmatch

`quickmatch` is currently under development and is not yet available on CRAN. The
development version and its dependencies can be installed with
[devtools](https://github.com/hadley/devtools) using the following code:
```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fsavje/quickmatch")
```

The package contains compiled code, and you must have a development environment
to install the development version. (Use `devtools::has_devel()` to check whether
you do.) If no development environment exists, Windows users download and install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) and macOS users download
and install [Xcode](https://itunes.apple.com/us/app/xcode/id497799835).

## Example

```R
# Construct example data
my_data <- data.frame(y = rnorm(100),
                      x1 = runif(100),
                      x2 = runif(100),
                      treatments = factor(sample(rep(c("T1", "T2", "C", "C"), 25))))

# Make distances
my_distances <- distances(my_data, dist_variables = c("x1", "x2"))

# Make matching with one unit from "T1", "T2" and "C" in each matched group
quickmatch(my_distances, my_data$treatments)

# Require at least two "C" in the groups
quickmatch(my_distances,
           my_data$treatments,
           treatment_constraints = c("T1" = 1, "T2" = 1, "C" = 2))

# Require groups with at least six units in total
quickmatch(my_distances,
           my_data$treatments,
           treatment_constraints = c("T1" = 1, "T2" = 1, "C" = 2),
           size_constraint = 6)

# Focus the matching to units assigned to T1 or T2.
# Each group will contain at least one unit of each treatment condition,
# but some "C" units might be unassigned.
quickmatch(my_distances,
           my_data$treatments,
           subset = c("T1", "T2"))

# Impose caliper
quickmatch(my_distances,
           my_data$treatments,
           caliper = 1.2)

# Call `quickmatch` directly with covariate data (ie., not pre-calculating distances)
quickmatch(my_data[c("x1", "x2")], my_data$treatments)
```
