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

#' Regression-based matching estimator of treatment effects
#'
#' \code{lm_match} estimates treatment effects in matched samples. The function
#' expects the user to provide the outcomes, treatment indicators, and a matching
#' object. It returns point estimates of the average treatment effects and variance
#' estimates. It is possible to estimate treatment effects for subsets of the
#' observations, such as estimates of the average treatment effect for the treated
#' (ATT).
#'
#' \code{lm_match} estimates treatment effects using weighted regression. The
#' function first derives the unit-level weights implied by the matching. In
#' detail, let \eqn{S(g)} be the number of units indicated by \code{target} in
#' group \eqn{g}. Let \eqn{T} be the total number of units indicated by
#' \code{target} in sample. Let \eqn{A(t, g)} be the number of units assigned
#' to treatment \eqn{t} in group \eqn{g}. The weight for a unit in group \eqn{g}
#' that is assigned to treatment \eqn{t} is given by:
#'
#' \deqn{\frac{S(g)}{T \times A(t, g)}.}{S(g) / [T * A(t, g)].}
#'
#' See \code{\link{matching_weights}} for more details.
#'
#' The function uses the derived weights in a weighted least squares regression
#' (using the \code{\link[stats]{lm}} function) with indicator variables for the
#' treatment conditions. Optionally, covariates can be added to the regression
#' (e.g., a common recommendation is to include the covariates used to construct
#' the matching). Standard errors are estimated with the heteroskedasticity-robust
#' "HC1" estimator in the \code{\link[sandwich]{vcovHC}} function. Units not
#' assigned to matched groups and units assigned weights of zero are excluded
#' from the estimation.
#'
#' @param outcomes
#'    numeric vector with observed outcomes.
#' @param treatments
#'    factor specifying the units' treatment assignments.
#' @param matching
#'    \code{\link{qm_matching}} or \code{\link[scclust]{scclust}} object with
#'    the matched groups.
#' @param covariates
#'    vector, matrix or data frame with covariates to include in the estimation.
#'    If \code{NULL}, no covariates are included.
#' @param target
#'    units to target the estimation for. If \code{NULL}, the effect is estimated
#'    for all units in the sample (i.e., ATE). A non-null value specifies a
#'    subset of units for which the effect should be estimated (e.g., ATT or
#'    ATC). If \code{target} is a logical vector with the same length as the
#'    sample size, units indicated with \code{TRUE} will be targeted. If
#'    \code{target} is an integer vector, the units with indices in \code{target}
#'    are targeted. If \code{target} is a character vector, it should contain
#'    treatment labels, and the effect for the corresponding units (as given by
#'    \code{treatments}) will be estimated.
#'
#' @return
#'    A list with two numeric matrices with all estimated treatment effects and
#'    their estimated variances is returned. The first matrix (\code{effects})
#'    contains estimated treatment effects. Rows in this matrix indicate minuends
#'    in the treatment effect contrast and columns indicate subtrahends. For
#'    example, in the matrix:
#'    \tabular{rrrr}{
#'      \tab a \tab b \tab c\cr
#'      a \tab 0.0 \tab 4.5 \tab 5.5\cr
#'      b \tab -4.5 \tab 0.0 \tab 1.0\cr
#'      c \tab -5.5 \tab -1.0 \tab 0.0\cr
#'    }
#'    the estimated treatment effect between conditions \eqn{a} and \eqn{b} is
#'    \eqn{4.5}, and the estimated treatment effect between conditions \eqn{c}
#'    and \eqn{b} is \eqn{-1.0}. In symbols, \eqn{E[Y(a) - Y(b) | S] = 4.5} and
#'    \eqn{E[Y(c) - Y(b) | S] = -1.0} where \eqn{S} is the condition set
#'    indicated by the \code{target} parameter.
#'
#'    The second matrix (\code{effect_variances}) contains estimates of
#'    variances of the corresponding effect estimators.
#'
#' @references
#'    Stuart, Elizabeth A. (2010),
#'    \sQuote{Matching Methods for Causal Inference: A Review and a Look Forward}.
#'    Statistical Science, 25(1), 1–21. https://doi.org/10.1214/09-STS313
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
#' # Make matching
#' my_matching <- quickmatch(my_distances, my_data$treatment)
#'
#' # ATE without covariates
#' lm_match(my_data$y,
#'          my_data$treatment,
#'          my_matching)
#'
#' # ATE with covariates
#' lm_match(my_data$y,
#'          my_data$treatment,
#'          my_matching,
#'          my_data[c("x1", "x2")])
#'
#' # ATT for T1
#' lm_match(my_data$y,
#'          my_data$treatment,
#'          my_matching,
#'          my_data[c("x1", "x2")],
#'          target = "T1")
#'
#' @export
lm_match <- function(outcomes,
                     treatments,
                     matching,
                     covariates = NULL,
                     target = NULL) {
  outcomes <- coerce_double(outcomes)
  num_observations <- length(outcomes)
  treatments <- coerce_treatments(treatments, num_observations)
  ensure_matching(matching, num_observations)
  covariates <- coerce_covariates(covariates, num_observations)
  target <- coerce_target(target, treatments)

  mwres <- internal_matching_weights(treatments, matching, target)

  if (sum(!mwres$treatment_missing) <= 1) {
    stop("Less than two potential outcomes can be estimated.")
  } else if (any(mwres$treatment_missing)) {
    warning("Some matched groups are missing treatment conditions. Corresponding potential outcomes cannot be estimated.")
  }

  # If weight is zero, set to NA
  mwres$unit_weights[mwres$unit_weights <= .Machine$double.eps] <- NA

  # No need to normalize with total number of units in `target` since regression
  # does it. Not normalizing is numerically more stable.
  if (is.null(covariates)) {
    lm_res <- stats::lm(outcomes ~ 0 + treatments, weights = mwres$unit_weights)
  } else {
    lm_res <- stats::lm(outcomes ~ 0 + treatments + covariates, weights = mwres$unit_weights)
  }
  var_res <- sandwich::vcovHC(lm_res, type = "HC1")

  est_treat <- which(!mwres$treatment_missing)
  out_te <- out_te_vars <- matrix(NA, nrow = nlevels(treatments), ncol = nlevels(treatments))
  dimnames(out_te) <- dimnames(out_te_vars) <- list(levels(treatments), levels(treatments))

  for (i in 1:length(est_treat)) {
    for (j in 1:length(est_treat)) {
      out_te[est_treat[i], est_treat[j]] <- lm_res$coefficients[i] - lm_res$coefficients[j]
      out_te_vars[est_treat[i], est_treat[j]] <- var_res[i, i] + var_res[j, j] - 2 * var_res[i, j]
    }
  }

  list(effects = out_te, effect_variances = out_te_vars)
}
