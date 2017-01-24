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
#' \code{potential_outcomes} estimates potential outcomes in matched groups. Provided
#' matched groups, outcomes and treatment indicators, the function returns estimates of the average
#' potential outcomes for all treatments for the units in the sample. It is also possible
#' to estimate potential outcomes for subsets of the units. For example, one can estimate
#' the potential outcomes for units assigned to a certain treatment condition.
#'
#'
#' @param outcomes    numeric vector with observed outcomes.
#'
#' @param treatments  integer or factor vector with treatment indicators.
#'
#' @param matching    \code{\link{qm_matching}} or \code{\link[Rscclust]{Rscc_clustering}}
#'                    object describing the matched groups.
#'
#' @param targets     vector of treatment labels (corresponding to \code{treatments})
#'                    specifying which potential outcomes to estimate. If \code{NULL},
#'                    all potential outcomes will be estimated.
#'
#' @param subset      units to estimate potential outcomes for. If \code{NULL},
#'                    the estimate will pertain to all units in the sample (i.e.,
#'                    corresponding to ATE). A non-null value specificies a subset
#'                    of units that the estimate should pertain to (e.g., ATT or ATC).
#'                    If \code{subset} is a logical vector and of the same length as the
#'                    sample size, units indicated with \code{TRUE} will be included.
#'                    Otherwise, \code{subset} should contain treatment labels, and
#'                    the corresponding units (as given by \code{treatments}) will be
#'                    included.
#'
#' @return Returns a named numeric vector with estimates of the potential outcomes.
#'
#' @seealso See \code{\link{treatment_effects}} for a treatment effects estimator.
#'
#' @examples
#' # Example input
#' outcomes <- (1:100)^0.5
#' treatments <- factor(rep(c("a", "b", "c", "c"), each = 25))
#' matching <- qm_matching(rep(1:10, 10))
#'
#' # Estimate all potential outcomes for the whole sample
#' potential_outcomes(outcomes, treatments, matching)
#'
#' # Estimate only the average potential outcome for the "a" treatment
#' potential_outcomes(outcomes, treatments, matching, targets = "a")
#'
#' # Estimate potential outcomes for units assigned to treatments "b" or "c"
#' potential_outcomes(outcomes, treatments, matching, subset = c("b", "c"))
#'
#' # Estimate potential outcomes for units 1:15 and 31:45
#' subset <- c(rep(TRUE, 15), rep(FALSE, 15), rep(TRUE, 15), rep(FALSE, 55))
#' potential_outcomes(outcomes, treatments, matching, subset = subset)
#'
#' @export
potential_outcomes <- function(outcomes,
                               treatments,
                               matching,
                               targets = NULL,
                               subset = NULL) {
  coerce_double(outcomes)
  num_observations <- length(outcomes)
  treatments <- Rscclust:::coerce_type_labels(treatments, num_observations)
  all_treatment_conditions <- get_all_treatment_conditions(treatments)
  Rscclust:::ensure_Rscc_clustering(matching, num_observations)

  if (is.null(targets)) {
    targets <- all_treatment_conditions
  }
  ensure_treatment_label_indicators(targets, all_treatment_conditions)

  if (is.character(subset)) {
    ensure_treatment_label_indicators(subset, all_treatment_conditions)
    subset <- Rscclust:::make_type_indicators(subset, treatments)
    subset <- translate_targets(subset, treatments)
  }
  subset <- Rscclust:::coerce_data_point_indices(subset, num_observations)

  internal_potential_outcomes(outcomes,
                              treatments,
                              matching,
                              targets,
                              subset)
}


# Potential outcome estimator C wrapper
#' @useDynLib quickmatch qmc_potential_outcomes
internal_potential_outcomes <- function(outcomes,
                                        treatments,
                                        matching,
                                        targets,
                                        subset) {
  targets <- Rscclust:::make_type_indicators(targets, treatments)

  if (is.logical(subset)) {
    subset <- which(subset) - 1L
  } else if (is.integer(subset)) {
    subset <- subset - 1L
  }

  ave_pot_outcomes <- .Call("qmc_potential_outcomes",
                            outcomes,
                            unclass(treatments),
                            matching,
                            targets,
                            subset,
                            PACKAGE = "quickmatch")

  ave_pot_outcomes <- ave_pot_outcomes[targets]
  names(ave_pot_outcomes) <- names(targets)[targets]
  ave_pot_outcomes
}


#' Average treatment effect estimator in matched groups
#'
#' \code{treatment_effects} estimates treatment effect in matched groups. Provided
#' matched groups, outcomes and treatment indicators, the function returns point estimates of the
#' average treatment effects for the units in the sample. It is also possible to estimate
#' treatment effects for subsets of the units. For example, one can estimate the effects
#' for units assigned to a certain treatment condition (e.g., ATT).
#'
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
#' @return Returns estimated treatment effects. If \code{contrasts} contains
#'         two conditions and \code{drop} is \code{TRUE}, a numeric scalar is returned
#'         with the corresponding treatment effect. In all other cases, a numeric matrix
#'         with all estimated treatment effects is returned. Rows in this matrix indicate minuends
#'         in the treatment effect contrast and columns indicate subtrahends. For example, in the matrix:
#'         \tabular{rrrr}{
#'         \tab a \tab b \tab c\cr
#'         a \tab 0.0 \tab 4.5 \tab 5.5\cr
#'         b \tab -4.5 \tab 0.0 \tab 1.0\cr
#'         c \tab -5.5 \tab -1.0 \tab 0.0\cr
#'         }
#'         the estimated treatment effect between conditions \eqn{a} and \eqn{b} is \eqn{4.5},
#'         and the estimated treatment effect between conditions \eqn{c} and \eqn{b} is \eqn{-1.0}.
#'         Or in symbols: \eqn{E[Y(a) - Y(b)] = 4.5} and \eqn{E[Y(c) - Y(b)] = -1.0}.
#'
#' @seealso \code{\link{potential_outcomes}} for a potential outcome estimator.
#'
#' @examples
#' # Example input
#' outcomes <- (1:100)^0.5
#' treatments <- factor(rep(c("a", "b", "c", "c"), each = 25))
#' matching <- qm_matching(rep(1:10, 10))
#'
#' # Estimate all treatment effects for the whole sample
#' treatment_effects(outcomes, treatments, matching)
#'
#' # Estimate only treatment effect between "a" and "b"
#' treatment_effects(outcomes, treatments, matching, contrasts = c("a", "b"))
#'
#' # As last command, but return a matrix
#' treatment_effects(outcomes, treatments, matching, contrasts = c("a", "b"), drop = FALSE)
#'
#' # Estimate all treatment effects for units assigned to treatments "b" or "c"
#' treatment_effects(outcomes, treatments, matching, subset = c("b", "c"))
#'
#' # Estimate all treatment effects for units 1:15 and 31:45
#' subset <- c(rep(TRUE, 15), rep(FALSE, 15), rep(TRUE, 15), rep(FALSE, 55))
#' treatment_effects(outcomes, treatments, matching, subset = subset)
#'
#' @export
treatment_effects <- function(outcomes,
                              treatments,
                              matching,
                              contrasts = NULL,
                              subset = NULL,
                              drop = TRUE) {
  coerce_double(outcomes)
  num_observations <- length(outcomes)
  treatments <- Rscclust:::coerce_type_labels(treatments, num_observations)
  all_treatment_conditions <- get_all_treatment_conditions(treatments)
  Rscclust:::ensure_Rscc_clustering(matching, num_observations)

  if (is.null(contrasts)) {
    targets <- all_treatment_conditions
  } else {
    targets <- contrasts
  }
  ensure_treatment_label_indicators(targets, all_treatment_conditions)

  if (is.character(subset)) {
    ensure_treatment_label_indicators(subset, all_treatment_conditions)
    subset <- Rscclust:::make_type_indicators(subset, treatments)
    subset <- translate_targets(subset, treatments)
  }
  subset <- Rscclust:::coerce_data_point_indices(subset, num_observations)

  Rscclust:::ensure_indicators(drop, 1L)

  po_vector <- internal_potential_outcomes(outcomes,
                                           treatments,
                                           matching,
                                           targets,
                                           subset)

  po_matrix <- as.matrix(po_vector) %*% t(as.matrix(rep(1, length(po_vector))))
  te <- po_matrix - t(po_matrix)
  dimnames(te) <- list(names(po_vector), names(po_vector))

  if (drop && (length(contrasts) == 2)) {
    te <- te[as.character(contrasts[1]), as.character(contrasts[2])]
    names(te) <- paste0(as.character(contrasts[1]), "-", as.character(contrasts[2]))
  }

  te
}
