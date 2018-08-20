# ==============================================================================
# quickmatch -- Quick Generalized Full Matching
# https://github.com/fsavje/quickmatch
#
# Copyright (C) 2018  Fredrik Savje and Jasjeet S. Sekhon
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

#' Covariate averages in matched sample
#'
#' \code{covariate_averages} derives covariate averages for treatment groups in
#' matched samples.
#'
#' \code{covariate_averages} calculates covariate averages by first deriving
#' the average for each covariate for each treatment conditions in each matched
#' group. It then aggregates the group averages by a weighted average, where the
#' \code{target} parameter decides the weights. If a matched group contains many
#' units not targeted (e.g., control units when ATT is of interest), those units
#' will contribute less to the covariate average for the corresponding treatment
#' condition than units in matched groups with many targeted units. This means that
#' the covariate average is calculated in the same way as the potential outcomes
#' are estimated. In fact, \code{covariate_averages} can be used as an estimator
#' for potential outcomes by calling it with the outcome variable as a covariate.
#'
#' When the average treatment effect (ATE) is of interest (i.e., \code{target == NULL}),
#' the matched groups will be weighted by their sizes. When \code{target} indicates
#' that some subset of units is of interest, the number of such units in each matched
#' group will decide its weight. For example, if we are interested in the average
#' treatment effect of the treated (ATT), the weight of a group will be proportional
#' to the number of treated units in that group.
#'
#' In practice, the function first derives the unit-level weights implied by the
#' matching. In detail, let \eqn{S(g)} be the number of units indicated by
#' \code{target} in group \eqn{g}. Let \eqn{T} be the total number of units
#' indicated by \code{target} in the sample. Let \eqn{A(t, g)} be the number of
#' units assigned to treatment \eqn{t} in group \eqn{g}. The weight for a unit
#' in group \eqn{g} that is assigned to treatment \eqn{t} is given by:
#'
#' \deqn{\frac{S(g)}{T \times A(t, g)}.}{S(g) / [T * A(t, g)].}
#'
#' See \code{\link{matching_weights}} for more details.
#'
#' \code{covariate_averages} focuses on means, but higher moments and interactions
#' can be investigated by adding corresponding columns to the covariate matrix
#' (see examples below).
#'
#' @param treatments
#'    factor specifying the units' treatment assignments.
#' @param covariates
#'    vector, matrix or data frame with covariates to calculate averages for.
#' @param matching
#'    \code{\link{qm_matching}} or \code{\link[scclust]{scclust}} object with
#'    the matched groups. If \code{NULL}, averages are derived for the unmatched
#'    sample.
#' @param target
#'    units to target the averages for. If \code{NULL}, the averages will
#'    be the raw average over all units in the sample (i.e., ATE). A non-null
#'    value specifies a subset of units to derive averages for (e.g.,
#'    ATT or ATC). If \code{target} is a logical vector with the same length as
#'    the sample size, units indicated with \code{TRUE} will be targeted. If
#'    \code{target} is an integer vector, the units with indices in \code{target}
#'    are targeted. If \code{target} is a character vector, it should contain
#'    treatment labels, and the corresponding units (as given by
#'    \code{treatments}) will be targeted. If \code{matching} is \code{NULL},
#'    \code{target} is ignored.
#'
#' @return
#'    Returns a matrix with the average of each covariate for each treatment
#'    group. The rows in the matrix correspond to the covariates in order and
#'    the columns correspond to the treatment groups. For example, a possible
#'    output with three treatment groups ("C", "T1" and "T2") and four
#'    covariates is:
#'    \tabular{rrr}{
#'      C \tab T1 \tab T2\cr
#'      3.0 \tab 3.3 \tab 3.5\cr
#'      -0.3 \tab 0.0 \tab -0.2\cr
#'      0.1 \tab 0.2 \tab 0.0\cr
#'      5.0 \tab 5.1 \tab 4.9\cr
#'    }
#'    which indicates that the average of the first covariate in the matched
#'    sample is 3.0 for units assigned to condition "C", and that the average
#'    of the third covariate is 0.2 for units assigned to condition "T1".
#'
#' @examples
#' # Construct example data
#' my_data <- data.frame(y = rnorm(100),
#'                       x1 = runif(100),
#'                       x2 = runif(100),
#'                       treatment = factor(sample(rep(c("T1", "T2", "C"), c(25, 25, 50)))))
#'
#' # Make distances
#' my_distances <- distances(my_data, dist_variables = c("x1", "x2"))
#'
#' # Treatment group averages in unmatched sample
#' covariate_averages(my_data$treatment, my_data[c("x1", "x2")])
#'
#' # Make matching
#' my_matching <- quickmatch(my_distances, my_data$treatment)
#'
#' # Treatment group averages in matched sample
#' covariate_averages(my_data$treatment, my_data[c("x1", "x2")], my_matching)
#'
#' # Averages in matched sample for ATT
#' covariate_averages(my_data$treatment,
#'                    my_data[c("x1", "x2")],
#'                    my_matching,
#'                    target = c("T1", "T2"))
#'
#' # Second-order moments and interactions
#' mod_covs <- data.frame(x1 = my_data$x1,
#'                        x2 = my_data$x2,
#'                        x1sq = my_data$x1^2,
#'                        x2sq = my_data$x2^2,
#'                        x1x2 = my_data$x1 * my_data$x2)
#' covariate_averages(my_data$treatment, mod_covs, my_matching)
#'
#' @export
covariate_averages <- function(treatments,
                               covariates,
                               matching = NULL,
                               target = NULL) {
  treatments <- coerce_treatments(treatments)
  num_observations <- length(treatments)
  covariates <- coerce_covariates(covariates, num_observations)
  if (is.null(covariates)) stop("`covariates` is NULL.")
  if (!is.null(matching)) ensure_matching(matching, num_observations)
  target <- coerce_target(target, treatments)

  if (is.null(matching)) {
    cov_tr_mean <- t(apply(covariates, 2, function(cov) {
      sapply(split(cov, treatments), mean)
    }))
  } else {
    mwres <- internal_matching_weights(treatments, matching, target, TRUE)
    if (any(mwres$treatment_missing)) {
      warning("Some matched groups are missing treatment conditions. Corresponding balance measures cannot be derived.")
    }
    cov_tr_mean <- t(apply(covariates, 2, function(cov) {
      sapply(split(cov * mwres$unit_weights, treatments), sum, na.rm = TRUE)
    }))
    cov_tr_mean <- cov_tr_mean / mwres$total_target_count
    cov_tr_mean[, mwres$treatment_missing] <- NA
  }

  cov_tr_mean
}
