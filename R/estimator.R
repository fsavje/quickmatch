# ==============================================================================
# quickmatch -- Fast Matching in Large Data Sets
# https://github.com/fsavje/quickmatch
#
# Copyright (C) 2016  Fredrik Savje -- http://fredriksavje.com
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


#' Average potential outcome estimator in matched groups
#'
#' \code{potential_outcomes} estimates potential outcomes using matched groups. For
#' an outcome, a matching and treatments, the function returns estimates of the
#' average potential outcomes for all treatments for the units in the sample. It is
#' possible to estimate potential outcomes for a subset of the units (e.g., to derive
#' ATT or ATC).
#'
#' @param outcomes    numeric vector with observed outcomes.
#'
#' @param treatments  integer or factor vector with treatment indicators.
#'
#' @param matching    qm_matching object containing the matching.
#'
#' @param estimands   vector of treatment labels (corresponding to \code{treatments})
#'                    specifying which potential outcomes to estimate. If \code{NULL},
#'                    all potential outcomes will be estimated.
#'
#' @param subset      units to estimate potential outcomes for. If \code{NULL},
#'                    the estimate will pertain to all units in the sample (i.e.,
#'                    corresponding to ATE). A non-null value specificies a subset
#'                    of units that the estimate should pertain to (e.g., ATT or ATC).
#'                    If \code{subset} is a logical vector and of length equal to the
#'                    sample size, units indicated with \code{TRUE} will be included.
#'                    Otherwise, \code{subset} should contain treatment labels and
#'                    the corresponding units (as given by \code{treatments}) will be
#'                    included.
#'
#' @return Returns a named numeric vector with an estimate of each potential outcome.
#'
#' @useDynLib quickmatch qmc_potential_outcomes
#' @export
potential_outcomes <- function(outcomes,
                               treatments,
                               matching,
                               estimands = NULL,
                               subset = NULL) {
  qm_check_numeric(outcomes)
  qm_check_treatment(treatments, length(outcomes))
  qm_check_matching(matching, length(outcomes))

  if (is.null(estimands)) estimands <- qm_get_all_treatment_conditions(treatments)
  estimands_indicators <- qm_get_treatment_indicators(estimands, treatments)

  subset_indicators <- NULL
  subset_treatments <- NULL
  if (!is.null(subset)) {
    if (is.logical(subset)) {
      stopifnot(length(outcomes) == length(subset))
      subset_indicators <- subset
    } else {
      subset_treatments <- qm_get_treatment_indicators(subset, treatments)
    }
  }

  ave_pot_outcomes <- .Call("qmc_potential_outcomes",
                            outcomes,
                            matching,
                            unclass(treatments),
                            estimands_indicators,
                            subset_indicators,
                            subset_treatments,
                            PACKAGE = "quickmatch")

  ave_pot_outcomes <- ave_pot_outcomes[estimands_indicators]
  names(ave_pot_outcomes) <- names(estimands_indicators)[estimands_indicators]
  ave_pot_outcomes
}
