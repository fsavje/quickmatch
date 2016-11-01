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


#' Construct matchings
#'
#' abc
#'
#' @param distances              abc
#'
#' @param treatments             abc
#'
#' @param treatment_constraints  abc
#'
#' @param total_size_constraint  abc
#'
#' @param caliper                abc
#'
#' @param subset                 abc
#'
#' @param ...                    abc
#'
#' @return abc
#'
#' @export
quickmatch <- function(distances,
                       treatments,
                       treatment_constraints = NULL,
                       total_size_constraint = NULL,
                       caliper = NULL,
                       subset = NULL,
                       ...) {
  ensure_distances(distances)
  num_observations <- get_distance_obs(distances)
  treatments <- coerce_labels(treatments)
  ensure_treatments(treatments, num_observations)
  all_treatment_conditions <- get_all_treatment_conditions(treatments)

  if (is.null(treatment_constraints)) {
    treatment_constraints <- rep(1L, length(all_treatment_conditions))
    names(treatment_constraints) <- as.character(all_treatment_conditions)
  }
  treatment_constraints <- coerce_integer(treatment_constraints)
  ensure_treatment_constraints(treatment_constraints)
  ensure_treatment_labels(names(treatment_constraints), all_treatment_conditions)

  if (is.null(total_size_constraint)) {
    total_size_constraint <- sum(treatment_constraints)
  }
  total_size_constraint <- coerce_integer(total_size_constraint)
  ensure_counts(total_size_constraint, 1L)

  ensure_caliper(caliper)

  if (!is.null(subset)) {
    if (!is.logical(subset)) {
      ensure_treatment_labels(subset, all_treatment_conditions)
      subset <- (treatments %in% subset)
    }
    ensure_indicators(subset, num_observations, any_true = TRUE)
  }

  out_matching <- Rscclust::nng_clustering_types(distance_object = distances,
                                                 type_labels = treatments,
                                                 type_size_constraints = treatment_constraints,
                                                 total_size_constraint = total_size_constraint,
                                                 main_radius = caliper,
                                                 main_data_points = subset,
                                                 ...)
  class(out_matching) <- c("qm_matching", class(out_matching))
  out_matching
}
