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
  Rscclust:::ensure_distances(distances)
  num_observations <- Rscclust:::data_point_count.Rscc_distances(distances)
  treatments <- Rscclust:::coerce_type_labels(treatments, num_observations)
  all_treatment_conditions <- get_all_treatment_conditions(treatments)

  if (is.null(treatment_constraints)) {
    treatment_constraints <- rep(1L, length(all_treatment_conditions))
    names(treatment_constraints) <- as.character(all_treatment_conditions)
  }
  treatment_constraints <- Rscclust:::coerce_type_constraints(treatment_constraints)
  ensure_treatment_label_indicators(names(treatment_constraints), all_treatment_conditions)

  total_size_constraint <- Rscclust:::coerce_total_size_constraint(total_size_constraint,
                                                                   treatment_constraints,
                                                                   num_observations)

  caliper <- Rscclust:::coerce_radius(caliper)

  if (is.logical(subset)) {
    Rscclust:::ensure_indicators(subset, num_observations, TRUE)
  } else if (!is.null(subset)) {
    ensure_treatment_label_indicators(subset, all_treatment_conditions)
    subset <- Rscclust:::make_type_indicators(subset, treatments)
    subset <- translate_targets(subset, treatments)
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
