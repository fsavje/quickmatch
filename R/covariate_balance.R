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

#' Covariate balance in matching
#'
#' ....
#'
#' @param treatments
#'    factor specifying which treatments the units are assigned to.
#' @param covariates
#'    vector, matrix or data frame with covariates to measure balance for.
#' @param matching
#'    \code{\link{qm_matching}} or \code{\link[scclust]{scclust}} object with
#'    the matched groups, or \code{NULL}.
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
#' @param normalize
#'    logical indicating whether differences should be normalized by the standard
#'    deviation of the covariate.
#' @param all_differences
#'    logical indicating whether full matrices of differences should be reported.
#'    If \code{FALSE}, only the maximum difference for each covariate is returned.
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
#'
#' @export
covariate_balance <- function(treatments,
                              covariates,
                              matching = NULL,
                              subset = NULL,
                              normalize = TRUE,
                              all_differences = FALSE) {
  treatments <- coerce_treatments(treatments)
  num_observations <- length(treatments)
  covariates <- coerce_covariates(covariates, num_observations)
  if (is.null(covariates)) {
    stop("`covariates` is NULL.")
  }
  normalize <- coerce_logical(normalize, 1L)
  all_differences <- coerce_logical(all_differences, 1L)

  if (is.null(matching)) {
    cov_tr_mean <- t(apply(covariates, 2, function(cov) {
      sapply(split(cov, treatments), mean)
    }))
  } else {
    mwres <- matching_weights(treatments, matching, subset)
    if (any(mwres$treatment_missing)) {
      warning("Some matched groups are missing treatment conditions. Corresponding balance measures cannot be derived.")
    }
    cov_tr_mean <- t(apply(covariates, 2, function(cov) {
      sapply(split(cov * mwres$unit_weights, treatments), sum)
    }))
    cov_tr_mean <- cov_tr_mean / mwres$total_subset_count
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
    list("diff_mats" = diff_mats,
         "max_diffs" = max_diffs)
  } else {
    max_diffs
  }
}
