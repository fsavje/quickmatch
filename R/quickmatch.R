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


#' Construct matched groups
#'
#' \code{quickmatch} constructs matched groups that satisfy matching constraints.
#'
#'
#' @param distances              an \code{Rscc_distances} object containing distances between
#'                               the units in the sample (i.e., measures on how similar the
#'                               units are). See \code{\link[Rscclust]{make_distances}} for
#'                               details on how to construct this object.
#'
#' @param treatments             integer or factor vector with treatment indicators.
#'
#' @param treatment_constraints  a named integer vector containing the matching constraints. If
#'                               \code{NULL}, the function defaults to requiring at least one unit
#'                               of each treatment condition in each matched group.
#'
#' @param total_size_constraint  an integer with the total size constraint of the matched
#'                               groups.
#'
#' @param subset                 units to target the matching for. All units indicated by \code{subset}
#'                               are ensured to be assigned to a matched group (disregarding eventual
#'                               \code{caliper} setting). Units not indicated by \code{subset} could
#'                               be left unassigned if they are not necessary to satisfy the matching
#'                               constraints. If \code{NULL}, \code{quickmatch} targets all units and
#'                               ensures that all units are assigned to a group. If \code{subset} is
#'                               a logical vector and of length equal to the
#'                               sample size, units indicated with \code{TRUE} will be included.
#'                               Otherwise, \code{subset} should contain treatment labels, and
#'                               the corresponding units (as given by \code{treatments}) will be
#'                               included.
#'
#' @param caliper                impose a caliper on the matching that restricts the maximum
#'                               allowed distance between units in the matching. The caliper
#'                               is imposed in the underlying data structure used by the algorithm,
#'                               and it does not directly correspond the maximum within-group
#'                               distance. See below for details.
#'
#' @param ...                    additional parameters to be sent the underlying
#'                               \code{\link[Rscclust]{nng_clustering_types}} function.
#'
#' @return Returns a \code{\link{qm_matching}} object describing the matched groups.
#'
#' @seealso See \code{\link[Rscclust]{nng_clustering_types}} for the underlying function used to
#'          construct the matched groups. See \code{\link{potential_outcomes}} and
#'          \code{\link{treatment_effects}} for estimators that can be used with the produced matching.
#'
#' @examples
#' my_data <- data.frame(y = rnorm(100),
#'                       x1 = runif(100),
#'                       x2 = runif(100),
#'                       treatment = factor(sample(rep(c("T", "C"), 50))))
#'
#' @export
quickmatch <- function(distances,
                       treatments,
                       treatment_constraints = NULL,
                       total_size_constraint = NULL,
                       subset = NULL,
                       caliper = NULL,
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
