# ==============================================================================
# quickmatch -- Quick Generalized Full Matching
# https://github.com/fsavje/quickmatch
#
# Copyright (C) 2017  Fredrik Savje and Jasjeet S. Sekhon
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

#' Covariate balance in matched sample
#'
#' \code{covariate_balance} derives measures of covariate balance between
#' treatment groups in matched samples. The function calculates normalized mean
#' differences between all pairs of treatment conditions for each covariate.
#'
#' \code{covariate_balance} calculates covariate balance by first deriving the
#' (normalized) mean difference between all treatment conditions for each
#' covariate in each matched group. It then aggregates the differences by a
#' weighted average, where the \code{target} parameter decides the weights.
#' When the average treatment effect (ATE) is of interest (i.e.,
#' \code{target == NULL}), the matched groups will be weighted by their sizes.
#' When \code{target} indicates that some subset of units is of interest, the
#' number of such units in each matched group will decide its weight. For
#' example, if we are interested in the average treatment effect of the treated
#' (ATT), the weight of a group will be proportional to the number of treated
#' units in that group. The reweighting of the groups captures that we are
#' prepared to accept greater imbalances in groups with few units of interest.
#'
#' By default, the differences are normalized by the sample standard deviation
#' of the corresponding covariate (see the \code{normalize} parameter). In more
#' detail, the sample variance of the covariate is derived separately for each
#' treatment group. The square root of the mean of these variances is then used
#' for the normalization. The matching is ignored when deriving the normalization
#' factor so that balance can be compared across different matchings or with
#' the unmatched sample.
#'
#' \code{covariate_balance} focuses on mean differences, but higher moments and
#' interactions can be investigated by adding corresponding columns to the
#' covariate matrix (see examples below).
#'
#' @param treatments
#'    factor specifying the units' treatment assignments.
#' @param covariates
#'    vector, matrix or data frame with covariates to derive balance for.
#' @param matching
#'    \code{\link{qm_matching}} or \code{\link[scclust]{scclust}} object with
#'    the matched groups. If \code{NULL}, balance is derived for the unmatched
#'    sample.
#' @param target
#'    units to target the balance measures for. If \code{NULL}, the measures will
#'    be the raw average over all units in the sample (i.e., ATE). A non-null
#'    value specifies a subset of units to derive balance measures for (e.g.,
#'    ATT or ATC). If \code{target} is a logical vector with the same length as
#'    the sample size, units indicated with \code{TRUE} will be targeted. If
#'    \code{target} is an integer vector, the units with indices in \code{target}
#'    are targeted. If \code{target} is a character vector, it should contain
#'    treatment labels, and the corresponding units (as given by
#'    \code{treatments}) will be targeted. If \code{matching} is \code{NULL},
#'    \code{target} is ignored.
#' @param normalize
#'    logical scalar indicating whether differences should be normalized by the
#'    sample standard deviation of the corresponding covariates.
#' @param all_differences
#'    logical scalar indicating whether full matrices of differences should be
#'    reported. If \code{FALSE}, only the maximum difference for each covariate
#'    is returned.
#'
#' @return
#'    Returns the mean difference between treatment groups in the matched sample
#'    for each covariate.
#'
#'    When \code{all_differences = TRUE}, the function returns a matrix for each
#'    covariate with the mean difference for each possible pair of treatment
#'    conditions. Rows in the matrices indicate minuends in the differences and
#'    columns indicate subtrahends. For example, when differences are normalized,
#'    the matrix:
#'    \tabular{rrrr}{
#'      \tab a \tab b \tab c\cr
#'      a \tab 0.0 \tab 0.3 \tab 0.5\cr
#'      b \tab -0.3 \tab 0.0 \tab 0.2\cr
#'      c \tab -0.5 \tab -0.2 \tab 0.0\cr
#'    }
#'    reports that the mean difference for the corresponding covariate between
#'    treatments "a" and "b" is 30\% of a sample standard deviation of the covariate.
#'    The maximum difference (in absolute value) is also reported in a separate vector.
#'    For example, the maximum difference for the covariate in the example above is 0.5.
#'
#'    When \code{all_differences = FALSE}, only the maximum differences are
#'    reported.
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
#' # Balance in unmatched sample (maximum for each covariate)
#' covariate_balance(my_data$treatment, my_data[c("x1", "x2")])
#'
#' # Make matching
#' my_matching <- quickmatch(my_distances, my_data$treatment)
#'
#' # Balance in matched sample (maximum for each covariate)
#' covariate_balance(my_data$treatment, my_data[c("x1", "x2")], my_matching)
#'
#' # Balance in matched sample for ATT
#' covariate_balance(my_data$treatment,
#'                   my_data[c("x1", "x2")],
#'                   my_matching,
#'                   target = c("T1", "T2"))
#'
#' # Balance on second-order moments and interactions
#' balance_cov <- data.frame(x1 = my_data$x1,
#'                           x2 = my_data$x2,
#'                           x1sq = my_data$x1^2,
#'                           x2sq = my_data$x2^2,
#'                           x1x2 = my_data$x1 * my_data$x2)
#' covariate_balance(my_data$treatment, balance_cov, my_matching)
#'
#' # Report all differences (not only maximum for each covariate)
#' covariate_balance(my_data$treatment,
#'                   my_data[c("x1", "x2")],
#'                   my_matching,
#'                   all_differences = TRUE)
#'
#' @export
covariate_balance <- function(treatments,
                              covariates,
                              matching = NULL,
                              target = NULL,
                              normalize = TRUE,
                              all_differences = FALSE) {
  treatments <- coerce_treatments(treatments)
  num_observations <- length(treatments)
  covariates <- coerce_covariates(covariates, num_observations)
  if (is.null(covariates)) {
    stop("`covariates` is NULL.")
  }
  if (!is.null(matching)) {
    ensure_matching(matching, num_observations)
    target <- coerce_target(target, treatments)
  }
  normalize <- coerce_logical(normalize, 1L)
  all_differences <- coerce_logical(all_differences, 1L)

  if (is.null(matching)) {
    cov_tr_mean <- t(apply(covariates, 2, function(cov) {
      sapply(split(cov, treatments), mean)
    }))
  } else {
    mwres <- internal_matching_weights(treatments, matching, target)
    if (any(mwres$treatment_missing)) {
      warning("Some matched groups are missing treatment conditions. Corresponding balance measures cannot be derived.")
    }
    cov_tr_mean <- t(apply(covariates, 2, function(cov) {
      sapply(split(cov * mwres$unit_weights, treatments), sum, na.rm = TRUE)
    }))
    cov_tr_mean <- cov_tr_mean / mwres$total_target_count
    cov_tr_mean[, mwres$treatment_missing] <- NA
  }

  if (normalize) {
    cov_sd <- apply(covariates, 2, function(cov) {
      sqrt(mean(sapply(split(cov, treatments), function(x) { ((length(x) - 1) / length(x)) * stats::var(x) })))
    })
    cov_tr_mean <- cov_tr_mean / cov_sd
  }

  v_one <- matrix(rep(1, ncol(cov_tr_mean)), nrow = 1)
  diff_mats <- lapply(split(cov_tr_mean, 1:nrow(cov_tr_mean)),
                      function(cov) {
                        out_diffs <- cov %*% v_one
                        out_diffs <- out_diffs - t(out_diffs)
                        dimnames(out_diffs) <- list(dimnames(cov_tr_mean)[[2]],
                                                    dimnames(cov_tr_mean)[[2]])
                        out_diffs
                      })

  max_diffs <- unlist(lapply(diff_mats, function(mat) { max(abs(mat), na.rm = TRUE) }))

  if (all_differences) {
    list("all_diffs" = diff_mats,
         "max_diffs" = max_diffs)
  } else {
    max_diffs
  }
}
