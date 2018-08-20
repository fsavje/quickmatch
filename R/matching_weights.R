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

#' Unit weights implied by matching
#'
#' \code{matching_weights} derives the weights implied by a matching for units
#' assigned to matched groups. If the matching is exact, reweighting the units
#' with this function will produce a sample where all treatment groups are
#' identical on the matching covariates. If the matching is approximate but of
#' good quality, the reweighted treatment groups will be close to identical.
#'
#' Let \eqn{S(g)} be the number of units indicated by \code{target} in group
#' \eqn{g} (or the total number of units in the group if \code{target} is
#' \code{NULL}). Let \eqn{T} be the total number of units indicated by
#' \code{target} in the sample (or the sample size if \code{target} is \code{NULL}).
#' Let \eqn{A(t, g)} be the number of units assigned to treatment \eqn{t} in
#' group \eqn{g}. The weight for a unit in group \eqn{g} that is assigned to
#' treatment \eqn{t} is given by:
#'
#' \deqn{\frac{S(g)}{T \times A(t, g)}.}{S(g) / [T * A(t, g)].}
#'
#' Consider, for example, a matched group with one treated unit and two control
#' units when we are interested in the average effect of the treated (ATT) and
#' we have 50 treated units in total (\eqn{T=50}). For all three units in the
#' group, we have \eqn{S(g)=1}. For the treated unit we have \eqn{A(t, g)=1},
#' so its weight becomes \eqn{1/50}. The two control units have \eqn{A(t, g)=2},
#' so their weights are both \eqn{1/100}.
#'
#' These weights are such that the difference between the weighted averages of
#' the outcomes in two treatment conditions is the same as the average
#' within-group difference-in-means between the two conditions.
#'
#' If a matched group \eqn{g} with \eqn{S(g) > 0} lacks some treatment condition
#' \eqn{t}, no weights exist for the units assigned to \eqn{t} (in other groups)
#' so to replicate group \eqn{g}; the matching does not contain enough
#' information to impute the missing treatment in group \eqn{g}. Subsequently,
#' all units assigned to \eqn{t} will be given the weight \code{NA}. There is
#' two ways to solve this problem. First, one can change the target estimand by
#' setting \eqn{S(g) = 0} for all groups that are missing units assigned to
#' \eqn{t}. This is done with the \code{target} parameter. Second, one can change
#' the matching so that all groups contain at least one unit assigned to \eqn{t}
#' (e.g., by merging groups).
#'
#' Units not assigned to matched groups are given zero weights.
#'
#' @param treatments
#'    factor specifying the units' treatment assignments.
#' @param matching
#'    \code{\link{qm_matching}} or \code{\link[scclust]{scclust}} object with
#'    the matched groups.
#' @param target
#'    units to target the weights for. If \code{NULL}, the weights will target
#'    all units, so that the reweighted treatment groups are as similar as
#'    possible to the complete sample (i.e., corresponding to ATE). A non-null
#'    value specifies a subset of units that the weights should be targeted for
#'    (e.g., ATT or ATC). If \code{target} is a logical vector with the same
#'    length as the sample size, units indicated with \code{TRUE} will be
#'    targeted. If \code{target} is an integer vector, the units with indices
#'    in \code{target} are targeted. If \code{target} is a character vector, it
#'    should contain treatment labels, and the weights target the corresponding
#'    units (as given by \code{treatments}).
#'
#' @return
#'    Returns a numeric vector with the weights of the units in the matching.
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
#' # Weights for ATE
#' weights_ate <- matching_weights(my_data$treatment, my_matching)
#'
#' # Weights for ATT for T1
#' weights_att <- matching_weights(my_data$treatment, my_matching, target = "T1")
#'
#' # Estimate treatment effects with WLS estimator (see `lm_match`)
#' effects <- lm(y ~ treatment + x1 + x2, data = my_data, weights = weights_att)
#'
#' @export
matching_weights <- function(treatments,
                             matching,
                             target = NULL) {
  treatments <- coerce_treatments(treatments)
  num_observations <- length(treatments)
  ensure_matching(matching, num_observations)
  target <- coerce_target(target, treatments)

  mwres <- internal_matching_weights(treatments, matching, target, FALSE)

  if (any(mwres$treatment_missing)) {
    warning("Some matched groups are missing treatment conditions. No weights exist for corresponding units.")
  }

  mwres$unit_weights / mwres$total_target_count
}
