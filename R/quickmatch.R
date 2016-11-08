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
#' \code{quickmatch} constructs matched groups satsifying specified matching
#' constraints. Provided distances measuring the similarity of the units in the sample
#' and a set of matching constraints, the function constructs a near-optimal
#' matching.
#'
#' The \code{treatment_constraints} argument should contain a named vector with all treatment-specific
#' constraints. For example, in a sample with treatment conditions "A", "B" and "C", the vector
#' \code{c("A" = 1, "B" = 2, "C" = 0)} specifies that each matched group should contain
#' at least one unit with treatment "A", at least two units with treatment "B" and any
#' number of units with treatment "C". Treatments not specified in the vector defaults to
#' zero. For example, the vector \code{c("A" = 1, "B" = 2)} is identical to the previous one.
#' When \code{NULL}, the parameter defaults to the unit vector. In our current example, \code{NULL} would
#' be shorthand for \code{c("A" = 1, "B" = 1, "C" = 1)}.
#'
#' The \code{total_size_constraint} argument can be used to require the matched groups to
#' contain units assigned to any treatment condition in addition to the constraints specified
#' in \code{treatment_constraints}. For example, if \code{treatment_constraints = c("A" = 1, "B" = 2)}
#' and \code{total_size_constraint = 4}, each match group will contain at least one unit assigned to "A",
#' at least two units assigned to "B" and at least four units in total, where the fourth unit can be from
#' any treatment condition.
#'
#' The \code{subset} argument can be used to control which units are included in the
#' matching. Under default settings, all units will be assigned to a matched group. In all other cases, the
#' argument indicates that some units can safely be ignored when the groups are constructed. This can be useful, for example,
#' when one is interested in estimating treatment effects only for a certain type of units (e.g.,
#' units assigned to a certain treatment condition). It is particularly useful when
#' units of interested are not represented in the whole covariate space (i.e., a one-sided overlap problem).
#' Without the \code{subset} argument, the function would in that case try to assign every unit to a group, including units in sparse regions that
#' we are not interested in. This could lead to unnecessarily large och diverse matched groups.
#'
#' As an example, assume there is two treatment conditions, "A" and "B", where units assigned to "B" are more
#' numerous and tend to have more extreme covariate values. We are, however, only interested in
#' estimate the treatment effect for units assigned to "A". By specifying \code{subset = "A"},
#' the function ensures that all those units are assigned to a matched group. Some units assigned
#' to treatment "B" -- in particular the units with extreme covariate values -- might be left unassigned.
#' However, as those units are not of interest, they can safely be ignored, and we thereby avoid groups with
#' poor qualities (i.e., groups that stretch over large distances in order to satisfy matching contraints).
#'
#' The default behavior when \code{subset} is set is to ignore units to the greatest possible
#' extent while satisfying the matching contraints. This tends to minimize within-group
#' distances which, in turn, tends to reduce the bias of treatment effect estimators. However, in order to
#' reduce variance, it is often beneficial to assign ignored units that are near existing matched groups to those groups.
#' This can be achieved using the \code{secondary_unassigned_method} and \code{secondary_radius} arguments
#' in the \code{\link[Rscclust]{nng_clustering_types}} function that \code{quickmatch} calls. The \code{subset}
#' argument corresponds to the \code{main_data_points} argument in \code{\link[Rscclust]{nng_clustering_types}}.
#' A similar, but more blunt, effect can be achieved by increasing \code{total_size_constraint}.
#'
#' The \code{caliper} argument bounds the maximum distance between units assigned to the same matched group.
#' This is implemented by restricting the edge weight in the graph used to construct the matched groups. As
#' a result, the caliper will affect all groups in the matching and, in general, make it harder for the function
#' to find good matchings also when the caliper is not binding. In particular, a too tight \code{caliper} can lead to discarded units that otherwise
#' would be assigned to a matched group satisfying both the matching constraints and the caliper. For this reason,
#' it is recommended to set \code{caliper} quite high and only use it to avoid particularly poor matches. It strongly
#' recommended to use the \code{caliper} argument only when \code{main_unassigned_method = "closest_seed"} in the
#' underlying \code{\link[Rscclust]{nng_clustering_types}} function (which is the default behavior). Other options
#' will still restrict the maximum within-group distance, but the bound is no longer guaranteed.
#'
#'
#' @param distances              an \code{Rscc_distances} object containing distances between
#'                               the units in the sample (i.e., measures on how similar the
#'                               units are). See \code{\link[Rscclust]{make_distances}} for
#'                               details on how to construct this object.
#'
#' @param treatments             integer or factor vector with treatment indicators.
#'
#' @param treatment_constraints  a named, integer vector with the treatment constraints.
#'                               If \code{NULL}, the constraints are set
#'                               to requiring one unit of each treatment condition in
#'                               each matched group.
#'
#' @param total_size_constraint  an integer with the total required number of units in each
#'                               matched group.
#'
#' @param subset                 units to target the matching for. All units indicated by \code{subset}
#'                               are ensured to be assigned to a matched group (disregarding eventual
#'                               \code{caliper} setting). Units not indicated by \code{subset} could
#'                               be left unassigned if they are not necessary to satisfy the matching
#'                               constraints. If \code{NULL}, \code{quickmatch} targets all units and
#'                               ensures that all units are assigned to a group. If \code{subset} is
#'                               a logical vector with the same length as the
#'                               sample size, units indicated with \code{TRUE} will be included.
#'                               Otherwise, \code{subset} should contain treatment labels, and
#'                               the corresponding units (as given by \code{treatments}) will be
#'                               included.
#'
#' @param caliper                restrict the maximum allowed distance between units in the matching.
#'
#' @param ...                    additional parameters to be sent the underlying
#'                               \code{\link[Rscclust]{nng_clustering_types}} function.
#'
#' @return Returns a \code{\link{qm_matching}} object with the matched groups.
#'
#' @seealso See \code{\link{potential_outcomes}} and \code{\link{treatment_effects}} for
#'          estimators that can be used with the produced matching.
#'
#'          See \code{\link[Rscclust]{nng_clustering_types}} for the underlying function used to
#'          construct the matched groups.
#'
#' @examples
#' # Construct example data
#' my_data <- data.frame(y = rnorm(100),
#'                       x1 = runif(100),
#'                       x2 = runif(100),
#'                       treatments = factor(sample(rep(c("T1", "T2", "C", "C"), 25))))
#'
#' # Make distances
#' my_distances <- Rscclust::make_distances(my_data, dist_variables = c("x1", "x2"))
#'
#' # Make matching with one unit from "T1", "T2" and "C" in each matched group
#' quickmatch(my_distances, my_data$treatments)
#'
#' # Require at least two "C" in the groups
#' quickmatch(my_distances,
#'            my_data$treatments,
#'            treatment_constraints = c("T1" = 1, "T2" = 1, "C" = 2))
#'
#' # Require at least six units in total in the groups
#' quickmatch(my_distances,
#'            my_data$treatments,
#'            total_size_constraint = 6)
#'
#' # Focus the matching to units assigned to T1 or T2.
#' # Each group will contain at least one of each treatment condition,
#' # but some "C" units might be unassigned.
#' quickmatch(my_distances,
#'            my_data$treatments,
#'            subset = c("T1", "T2"))
#'
#' # Impose caliper
#' quickmatch(my_distances,
#'            my_data$treatments,
#'            caliper = 0.5)
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

  if (is.logical(subset)) {
    Rscclust:::ensure_indicators(subset, num_observations, TRUE)
  } else if (!is.null(subset)) {
    ensure_treatment_label_indicators(subset, all_treatment_conditions)
    subset <- Rscclust:::make_type_indicators(subset, treatments)
    subset <- translate_targets(subset, treatments)
  }

  caliper <- coerce_caliper(caliper)
  dots <- eval(substitute(alist(...)))
  ensure_sane_caliper(caliper, dots$main_unassigned_method)

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
