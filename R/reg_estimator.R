# ==============================================================================
# quickmatch -- Quick Generalized Full Matching
# https://github.com/fsavje/quickmatch
#
# Copyright (C) 2017  Fredrik Savje -- http://fredriksavje.com
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/
# ==============================================================================

#' Regression-based average treatment effect estimator
#'
#' \code{regression_estimator} estimates treatment effects from matched groups.
#' Provided a matching, outcomes and treatment indicators, the function returns
#' point estimates of the average treatment effects for the units in the sample
#' and variance estimates for those effects. It is also possible to estimate
#' treatment effects for subsets of the units. For example, one can estimate the
#' effects for units assigned to a certain treatment condition (e.g., ATT).
#'
#' @param outcomes
#'    numeric vector with observed outcomes.
#' @param treatments
#'    factor specifying which treatments the units are assigned to.
#' @param matching
#'    \code{\link{qm_matching}} or \code{\link[scclust]{scclust}} object with
#'    the matched groups.
#' @param covariates
#'    vector, matrix or data frame with covariates to include in the estimation.
#'    If \code{NULL}, no covariates are included.
#' @param subset
#'    units to target the estimation for. If \code{NULL}, the estimate will
#'    pertain to all units in the sample (i.e., ATE). A non-null value
#'    specificies a subset of units that the estimate should pertain to (e.g.,
#'    ATT or ATC). If \code{subset} is a logical vector with the same length as
#'    the sample size, units indicated with \code{TRUE} will be included. If
#'    \code{subset} is an integer vector, the units with indices in \code{subset}
#'    are included. Indices starts at 1 and \code{subset} must be sorted. If
#'    \code{subset} is a character vector, it should contain treatment labels,
#'    and the corresponding units (as given by \code{treatments}) will be
#'    included in the estimation.
#'
#' @return
#'    Returns estimated treatment effects. A list with two numeric matrix with
#'    all estimated treatment effects and their estimated variances is returned.
#'    The first matrix (\code{effects}) contains estimated treatment effects.
#'    Rows in this matrix indicate minuends in the treatment effect contrast and
#'    columns indicate subtrahends. For example, in the matrix:
#'    \tabular{rrrr}{
#'      \tab a \tab b \tab c\cr
#'      a \tab 0.0 \tab 4.5 \tab 5.5\cr
#'      b \tab -4.5 \tab 0.0 \tab 1.0\cr
#'      c \tab -5.5 \tab -1.0 \tab 0.0\cr
#'    }
#'    the estimated treatment effect between conditions \eqn{a} and \eqn{b} is
#'    \eqn{4.5}, and the estimated treatment effect between conditions \eqn{c}
#'    and \eqn{b} is \eqn{-1.0}. Or in symbols: \eqn{E[Y(a) - Y(b)] = 4.5} and
#'    \eqn{E[Y(c) - Y(b)] = -1.0}.
#'
#'    The second matrix (\code{effect_variances}) contains estimates of
#'    variances of the corresponding effect estimators.
#'
#' @examples
#' # Construct example data
#' my_data <- data.frame(y = rnorm(100),
#'                       x1 = runif(100),
#'                       x2 = runif(100),
#'                       treatments = factor(sample(rep(c("T1", "T2", "C", "C"), 25))))
#'
#' # Make distances
#' my_distances <- distances(my_data, dist_variables = c("x1", "x2"))
#'
#' # Make matching
#' my_matching <- quickmatch(my_distances, my_data$treatments)
#'
#' # ATE
#' regression_estimator(my_data$y, my_data$treatments, my_matching, my_data[c("x1", "x2")])
#'
#' # ATT for T1
#' regression_estimator(my_data$y, my_data$treatments, my_matching, my_data[c("x1", "x2")], "T1")
#'
#' @export
regression_estimator <- function(outcomes,
                                 treatments,
                                 matching,
                                 covariates = NULL,
                                 subset = NULL) {
  outcomes <- coerce_double(outcomes)
  num_observations <- length(outcomes)
  treatments <- coerce_treatments(treatments, num_observations)
  ensure_matching(matching, num_observations)
  covariates <- coerce_covariates(covariates, num_observations)
  subset <- coerce_subset(subset, treatments)

  if (is.null(subset)) {
    subset <- rep(TRUE, num_observations)
  } else if (is.integer(subset)) {
    tmp_subset <- rep(FALSE, num_observations)
    tmp_subset[subset] <- TRUE
    subset <- tmp_subset
  }

  # Find matched groups with treatments
  subset_match <- unique(as.integer(matching)[subset])
  treatment_missing <- !unlist(lapply(split(as.integer(matching), treatments, drop = FALSE),
                                      function(x) { all(subset_match %in% unique(x)) }))
  if (any(treatment_missing)) {
    warning("Some matched groups are missing treatment conditions. Corresponding potential outcomes cannot be estimated.")
  }

  # Estimation
  match_treat_factor <- interaction(as.integer(matching), treatments)

  subset_count <- rep(NA, num_observations)
  split(subset_count, as.integer(matching)) <- lapply(split(subset, as.integer(matching)), sum)

  match_treat_count <- rep(NA, num_observations)
  split(match_treat_count, match_treat_factor) <- lapply(split(outcomes, match_treat_factor), length)

  # No need to normalize with total number of units in `subset` since regression
  # does it. Not normalizing is numerically more stable.
  reg_weights <- subset_count / match_treat_count

  if (is.null(covariates)) {
    lm_res <- stats::lm(outcomes ~ 0 + treatments, weights = reg_weights)
  } else {
    lm_res <- stats::lm(outcomes ~ 0 + treatments + covariates, weights = reg_weights)
  }

  coef <- lm_res$coefficients[1:nlevels(treatments)]
  coef_var <- sandwich::vcovHC(lm_res, type = "HC1")[1:nlevels(treatments), 1:nlevels(treatments)]

  coef[treatment_missing] <- NA
  coef_var[treatment_missing, ] <- NA
  coef_var[, treatment_missing] <- NA

  out_means <- out_vars <- matrix(NA, nrow = nlevels(treatments), ncol = nlevels(treatments))
  dimnames(out_means) <- dimnames(out_vars) <- list(levels(treatments), levels(treatments))

  for (i in 1:nlevels(treatments)) {
    for (j in 1:nlevels(treatments)) {
      out_means[i, j] <- coef[i] - coef[j]
      out_vars[i, j] <- coef_var[i, i] + coef_var[j, j] - 2 * coef_var[i, j]
    }
  }

  list(effects = out_means, effect_variances = out_vars)
}
