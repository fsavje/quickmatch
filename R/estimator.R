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
#' \code{potential_outcomes} estimates potential outcomes in matched groups. For
#' an outcome, a matching and treatments, the function returns estimates of the
#' average potential outcomes for all treatments for the units in the sample. It is
#' possible to estimate potential outcomes for a subset of the units (e.g., ATT or
#' ATC).
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
  coerce_double(outcomes)
  treatments <- Rscclust:::coerce_type_labels(treatments, length(outcomes))
  all_treatment_conditions <- get_all_treatment_conditions(treatments)
  Rscclust:::ensure_Rscc_clustering(matching, length(outcomes))

  if (is.null(estimands)) {
    estimands <- all_treatment_conditions
  }
  ensure_treatment_label_indicators(estimands, all_treatment_conditions)

  if (is.logical(subset)) {
    Rscclust:::ensure_indicators(subset, length(outcomes), TRUE)
  } else if (!is.null(subset)) {
    ensure_treatment_label_indicators(subset, all_treatment_conditions)
  }

  internal_potential_outcomes(outcomes,
                              treatments,
                              matching,
                              estimands,
                              subset)
}


# C wrapper
internal_potential_outcomes <- function(outcomes,
                                        treatments,
                                        matching,
                                        estimands,
                                        subset) {
  estimands <- Rscclust:::make_type_indicators(estimands, treatments)

  if (!is.null(subset) && !is.logical(subset)) {
    subset <- Rscclust:::make_type_indicators(subset, treatments)
    subset <- translate_targets(subset, treatments)
  }

  ave_pot_outcomes <- .Call("qmc_potential_outcomes",
                            outcomes,
                            unclass(treatments),
                            matching,
                            estimands,
                            subset,
                            PACKAGE = "quickmatch")

  ave_pot_outcomes <- ave_pot_outcomes[estimands]
  names(ave_pot_outcomes) <- names(estimands)[estimands]
  ave_pot_outcomes
}


#' Average treatment effect estimator in matched groups
#'
#' \code{treatment_effects} estimates treatment effect in matched groups. For a
#' set of treatment conditions, the function returns estimates of treatment
#' effects for the units in the sample for each pair of conditions.
#' It is possible to estimate treatment effects for a subset of the
#' units (e.g., ATT or ATC).
#'
#' @param contrasts   vector of treatment labels (corresponding to \code{treatments})
#'                    specifying which treatment effects to estimate. If \code{NULL},
#'                    all treatment effects will be estimated.
#'
#' @param drop        if \code{FALSE}, the function always returns a matrix (even when
#'                    \code{contrasts} contains two elements).
#'
#' @inheritParams potential_outcomes
#'
#' @return Returns the estimated treatment effects. If \code{contrasts} is of length
#'         two, a scaler is returned with the corresponding treatment effect. In all
#'         other cases, a numeric matrix with all treatment effects are returned. Rows
#'         in this matrix indicate minuends in the treatment contrast and columns
#'         indicate subtrahends.
#'
#' @useDynLib quickmatch qmc_potential_outcomes
#' @export
treatment_effects <- function(outcomes,
                              treatments,
                              matching,
                              contrasts = NULL,
                              subset = NULL,
                              drop = TRUE) {
  coerce_double(outcomes)
  treatments <- Rscclust:::coerce_type_labels(treatments, length(outcomes))
  all_treatment_conditions <- get_all_treatment_conditions(treatments)
  Rscclust:::ensure_Rscc_clustering(matching, length(outcomes))

  if (is.null(contrasts)) {
    contrasts <- all_treatment_conditions
  }
  ensure_treatment_label_indicators(contrasts, all_treatment_conditions)

  if (is.logical(subset)) {
    Rscclust:::ensure_indicators(subset, length(outcomes), TRUE)
  } else if (!is.null(subset)) {
    ensure_treatment_label_indicators(subset, all_treatment_conditions)
  }

  Rscclust:::ensure_indicators(drop, 1L)

  po_vector <- internal_potential_outcomes(outcomes = outcomes,
                                           treatments = treatments,
                                           matching = matching,
                                           estimands = contrasts,
                                           subset = subset)

  po_matrix <- as.matrix(po_vector) %*% t(as.matrix(rep(1, length(po_vector))))
  te <- po_matrix - t(po_matrix)
  dimnames(te) <- list(names(po_vector), names(po_vector))

  if (drop && (length(po_vector) == 2)) {
    te <- te[1, 2]
    names(te) <- paste0(names(po_vector), collapse = "-")
  }

  te
}
