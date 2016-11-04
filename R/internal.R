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


# Get all treatment conditions that is represented in `treatments`
get_all_treatment_conditions <- function(treatments) {
  stopifnot(is.factor(treatments) || is.integer(treatments))
  if (is.factor(treatments)) {
    levels(treatments)
  } else if (is.integer(treatments)) {
    sort(unique(treatments))
  }
}


# ==============================================================================
# C wrappers
# ==============================================================================

# Estimate potential outcomes
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


# Translate treatment labels to indicators for each unit
# translate_targets(c(TRUE, FALSE, TRUE),
#                   c(0L, 0L, 1L, 2L, 1L, 0L))
# > c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
translate_targets <- function(targets,
                              treatments) {
  stopifnot(is.logical(targets),
            is.factor(treatments) || is.integer(treatments))
  .Call("qmc_translate_targets",
        targets,
        unclass(treatments),
        PACKAGE = "quickmatch")
}
