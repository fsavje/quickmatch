# quickmatch

[![Build Status](https://travis-ci.org/fsavje/quickmatch.svg?branch=master)](https://travis-ci.org/fsavje/quickmatch)
[![Build status](https://ci.appveyor.com/api/projects/status/beypek5qq868d4yf/branch/master?svg=true)](https://ci.appveyor.com/project/fsavje/quickmatch/branch/master)
[![codecov](https://codecov.io/gh/fsavje/quickmatch/branch/master/graph/badge.svg)](https://codecov.io/gh/fsavje/quickmatch)

`quickmatch` provides matching functions for constructing generalized full matchings in large samples. It also provides computationally efficient estimators for treatment effects and potential outcomes in matched groups.

## Install quickmatch

`quickmatch` is currently under development and is not yet available on CRAN. The development version and its dependencies can be installed with [devtools](https://github.com/hadley/devtools) using the following:
```R
# Install devtools, if not already installed, with command:
# install.packages("devtools")

# Install quickmatch
devtools::install_github("fsavje/quickmatch")
```

## Example

```R
# Construct example data
my_data <- data.frame(y = rnorm(100),
                      x1 = runif(100),
                      x2 = runif(100),
                      treatments = factor(sample(rep(c("T1", "T2", "C", "C"), 25))))

# Make distances
my_distances <- Rscclust::make_distances(my_data, dist_variables = c("x1", "x2"))

# Make matching with one unit from "T1", "T2" and "C" in each matched group
quickmatch(my_distances, my_data$treatments)

# Require at least two "C" in the groups
quickmatch(my_distances,
           my_data$treatments,
           treatment_constraints = c("T1" = 1, "T2" = 1, "C" = 2))

# Require at least six units in total in the groups
quickmatch(my_distances,
           my_data$treatments,
           total_size_constraint = 6)

# Focus the matching to units assigned to T1 or T2.
# Each group will contain at least one of each treatment condition,
# but some "C" units might be unassigned.
quickmatch(my_distances,
           my_data$treatments,
           subset = c("T1", "T2"))
```
