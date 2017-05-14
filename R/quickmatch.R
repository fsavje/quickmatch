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

#' Derive generalized full matchings
#'
#' \code{quickmatch} constructs near-optimal generalized full matchings. The
#' function expects the user to provide distances measuring the similarity of
#' units and a set of matching constraints. It then constructs a matching
#' so that units assigned to the same group are as similar as possible while
#' satisfying the matching constraints.
#'
#' The \code{treatment_constraints} parameter should be a named vector with
#' treatment-specific constraints. For example, in a sample with treatment
#' conditions "A", "B" and "C", the vector \code{c("A" = 1, "B" = 2, "C" = 0)}
#' specifies that each matched group should contain at least one unit with
#' treatment "A", at least two units with treatment "B" and any number of units
#' with treatment "C". Treatments not specified in the vector defaults to zero.
#' For example, the vector \code{c("A" = 1, "B" = 2)} is identical to the
#' previous one. When \code{treatment_constraints} is \code{NULL}, the function
#' requires at least one unit for each treatment in each group. In our current
#' example, \code{NULL} would be shorthand for \code{c("A" = 1, "B" = 1, "C" = 1)}.
#'
#' The \code{size_constraint} parameter can be used to constrain the matched
#' groups to contain at least a certain number of units in total (independently
#' of treatment assignment). For example, if \code{treatment_constraints =
#' c("A" = 1, "B" = 2)} and \code{total_size_constraint = 4}, each matched
#' group will contain at least one unit assigned to "A", at least two units
#' assigned to "B" and at least four units in total, where the fourth unit can
#' be from any treatment condition.
#'
#' The \code{target} parameter can be used to control which units are included
#' in the matching. When \code{target} is \code{NULL} (the default), all units
#' will be assigned to a matched group. When not \code{NULL}, the parameter
#' indicates that some units must be assigned to matched group and that the
#' remaining units can safely be ignored. This can be useful, for example,
#' when one is interested in estimating treatment effects only for a certain
#' type of units (e.g., the average treatment effect for the treated, ATT). It
#' is particularly useful when units of interested are not represented in the
#' whole covariate space (i.e., an one-sided overlap problem). Without the
#' \code{target} parameter, the function would in such cases try to assign every
#' unit to a group, including units in sparse regions that we are not interested
#' in. This could lead to unnecessarily large and diverse matched groups. By
#' specifying that some units are of interest only insofar as they help us satisfy
#' the matching constraints (i.e., setting the \code{target} parameter to the
#' appropriate value), we can avoid such situations.
#'
#' Consider, as an example, a study with two treatment conditions, "A" and "B".
#' Units assigned to "B" are more numerous and tend to have more extreme
#' covariate values. We are, however, only interested in estimating the
#' treatment effect for units assigned to "A". By specifying \code{target = "A"},
#' the function ensures that all "A" units are assigned to matched groups. Some
#' units assigned to treatment "B" -- in particular the units with extreme
#' covariate values -- will be left unassigned. However, as those units are not
#' of interest, they can safely be ignored, and we avoid groups of poor quality.
#'
#' Even if some of the units that can be ignored are not needed to satisfy the
#' matching constraints, it is rarely beneficial to discard them blindly; they can
#' occasionally provide useful information. The default behavior when \code{target}
#' is non-NULL is to assign as many of the ignorable units as possible given that
#' the within-group distances do not increase too much
#' (using \code{secondary_unassigned_method = "estimated_radius"}). This behavior
#' might, however, reduce covariate balance in some instances. If called with
#' \code{secondary_unassigned_method = "ignore"}, units not specified in
#' \code{target} will be discarded unless they are absolutely needed to satisfying
#' the matching constraints. This tends to reduce bias since the within-group
#' distances are minimized, but it could increase variance since we ignore
#' potentially useful information in the sample. An intermediate alternative
#' is to specify an aggressive caliper for the ignorable units, which is done
#' with the \code{secondary_radius} parameter. (These parameters are part of the
#' \code{\link[scclust]{sc_clustering}} function that \code{quickmatch} calls.
#' The \code{target} parameter corresponds to the \code{primary_data_points}
#' parameter in that function.)
#'
#' The \code{caliper} parameter constrains the maximum distance between units
#' assigned to the same matched group. This is implemented by restricting the
#' edge weight in the graph used to construct the matched groups (see
#' \code{\link[scclust]{sc_clustering}} for details). As a result, the caliper
#' will affect all groups in the matching and, in general, make it harder for
#' the function to find good matches even for groups where the caliper is not
#' binding. In particular, a too tight \code{caliper} can lead to discarded
#' units that otherwise would be assigned to a group satisfying both the
#' matching constraints and the caliper. For this reason, it is recommended
#' to set the \code{caliper} value quite high and only use it to avoid particularly
#' poor matches. It strongly recommended to use the \code{caliper} parameter only
#' when \code{primary_unassigned_method = "closest_seed"} in the underlying
#' \code{\link[scclust]{sc_clustering}} function (which is the default
#' behavior).
#'
#' @param distances
#'    \code{\link[distances]{distances}} object or a numeric vector, matrix
#'    or data frame. The parameter describes the similarity of the units to be
#'    matched. It can either be preprocessed distance information using a
#'    \code{\link[distances]{distances}} object, or raw covariate data. When
#'    called with covariate data, Euclidean distances are calculated unless
#'    otherwise specified.
#' @param treatments
#'    factor specifying the units' treatment assignments.
#' @param treatment_constraints
#'    named integer vector with the treatment constraints. If \code{NULL}, the
#'    function ensures that each matched group contains one unit from each
#'    treatment condition.
#' @param size_constraint
#'    integer with the required total number of units in each group. Must be
#'    greater or equal to the sum of \code{treatment_constraints}. If NULL, no
#'    constraints other than the treatment constraints are imposed.
#' @param target
#'    units to target the matching for. All units indicated by \code{target} are
#'    ensured to be assigned to a matched group (disregarding eventual
#'    \code{caliper} setting). Units not indicated by \code{target} could be
#'    left unassigned if they are not necessary to satisfy the matching
#'    constraints. If \code{NULL}, \code{quickmatch} targets the complete sample
#'    and ensures that all units are assigned to a group. If \code{target} is a
#'    logical vector with the same length as the sample size, units indicated
#'    with \code{TRUE} will be targeted. If \code{target} is an integer vector,
#'    the units with indices in \code{target} are targeted. Indices starts at 1
#'    and \code{target} must be sorted. If \code{target} is a character vector,
#'    it should contain treatment labels, and the corresponding units (as given
#'    by \code{treatments}) will be targeted.
#' @param caliper
#'    restrict the maximum within-group distance.
#' @param ...
#'    additional parameters to be sent either to the \code{\link[distances]{distances}}
#'    function when the \code{distances} parameter contains covariate data, or
#'    to the underlying \code{\link[scclust]{sc_clustering}} function.
#'
#' @return
#'    Returns a \code{\link{qm_matching}} object with the matched groups.
#'
#' @seealso
#'   See \code{\link[scclust]{sc_clustering}} for the underlying function used
#'   to construct the matched groups.
#'
#' @references
#' Sävje, Fredrik, Michael J. Higgins and Jasjeet S. Sekhon (2017),
#' \sQuote{Generalized Full Matching}, arXiv 1703.03882.
#' \url{https://arxiv.org/abs/1703.03882}
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
#' # Make matching with one unit from "T1", "T2" and "C" in each matched group
#' quickmatch(my_distances, my_data$treatment)
#'
#' # Require at least two "C" in each group
#' quickmatch(my_distances,
#'            my_data$treatment,
#'            treatment_constraints = c("T1" = 1, "T2" = 1, "C" = 2))
#'
#' # Require groups with at least six units in total
#' quickmatch(my_distances,
#'            my_data$treatment,
#'            treatment_constraints = c("T1" = 1, "T2" = 1, "C" = 2),
#'            size_constraint = 6)
#'
#' # Focus the matching to units assigned to "T1" and "T2" (i.e., all
#' # units assigned to "T1" or T2 will be assigned to a matched group).
#' # Units assigned to treatment "C" will be assigned to groups so to
#' # ensure that each group contains at least one unit of each treatment
#' # condition. Remaining "C" units could be left unassigned.
#' quickmatch(my_distances,
#'            my_data$treatment,
#'            target = c("T1", "T2"))
#'
#' # Impose caliper
#' quickmatch(my_distances,
#'            my_data$treatment,
#'            caliper = 0.25)
#'
#' # Call `quickmatch` directly with covariate data (ie., not pre-calculating distances)
#' quickmatch(my_data[c("x1", "x2")], my_data$treatment)
#'
#' # Call `quickmatch` directly with covariate data using Mahalanobis distances
#' quickmatch(my_data[c("x1", "x2")],
#'            my_data$treatment,
#'            normalize = "mahalanobize")
#'
#' @export
quickmatch <- function(distances,
                       treatments,
                       treatment_constraints = NULL,
                       size_constraint = NULL,
                       target = NULL,
                       caliper = NULL,
                       ...) {
  dots <- eval(substitute(alist(...)))

  if (!distances::is.distances(distances)) {
    dist_call <- dots[names(dots) %in% names(formals(distances::distances))]
    dist_call$data <- distances
    distances <- do.call(distances::distances, dist_call)
  }
  ensure_distances(distances)

  num_observations <- length(distances)
  treatments <- coerce_treatments(treatments, num_observations, FALSE)

  if (is.null(treatment_constraints)) {
    treatment_constraints <- rep(1L, nlevels(treatments))
    names(treatment_constraints) <- levels(treatments)
  }
  treatment_constraints <- coerce_treatment_constraints(treatment_constraints,
                                                        levels(treatments))

  size_constraint <- coerce_size_constraint(size_constraint,
                                            sum(treatment_constraints),
                                            num_observations)

  target <- coerce_target(target, treatments, FALSE)
  ensure_caliper(caliper)

  sc_call <- dots[names(dots) %in% names(formals(scclust::sc_clustering))]

  if (!is.null(sc_call$type_labels)) {
    stop("`type_labels` is ignored, please use the `treatments` parameter instead.")
  }
  if (!is.null(sc_call$type_constraints)) {
    stop("`type_constraints` is ignored, please use the `treatment_constraints` parameter instead.")
  }
  if (!is.null(sc_call$primary_data_points)) {
    stop("`primary_data_points` is ignored, please use the `target` parameter instead.")
  }
  if (is.null(sc_call$primary_unassigned_method)) {
    sc_call$primary_unassigned_method <- "closest_seed"
  }
  if (is.null(sc_call$secondary_unassigned_method)) {
    sc_call$secondary_unassigned_method <- "closest_seed"
  }
  if (is.null(sc_call$primary_radius)) {
    sc_call$primary_radius <- "seed_radius"
  }
  if (is.null(sc_call$secondary_radius)) {
    if (is.null(caliper)) {
      sc_call$secondary_radius <- "estimated_radius"
    } else {
      sc_call$secondary_radius <- "seed_radius"
    }
  }

  # If `caliper` is NULL, do nothing
  # If `sc_call$seed_radius` is NULL, use `caliper`
  if (is.null(sc_call$seed_radius) && !is.null(caliper)) {
    if (sc_call$primary_unassigned_method %in% c("ignore", "closest_seed")) {
      sc_call$seed_radius <- as.numeric(caliper) / 2.0
      if (sc_call$secondary_unassigned_method == "closest_assigned") {
        warning("Caliper is not properly enforced when `secondary_unassigned_method`==\"closest_assigned\".")
      }
    } else if (sc_call$primary_unassigned_method %in% c("any_neighbor", "closest_assigned")) {
      sc_call$seed_radius <- as.numeric(caliper) / 4.0
      warning("Caliper might perform poorly unless `primary_unassigned_method`==\"closest_seed\".")
    }
    if (sc_call$primary_radius != "seed_radius") {
      warning("Caliper is not properly enforced unless `primary_radius`==\"seed_radius\".")
    }
    if (sc_call$secondary_radius != "seed_radius") {
      warning("Caliper is not properly enforced unless `secondary_radius`==\"seed_radius\".")
    }
  } else if (!is.null(sc_call$seed_radius) && !is.null(caliper)) {
    warning("`caliper` is ignored when `seed_radius` is specified.")
  }

  sc_call$distances <- distances
  sc_call$size_constraint <- size_constraint
  sc_call$type_labels <- treatments
  sc_call$type_constraints <- treatment_constraints
  sc_call$primary_data_points <- target

  out_matching <- do.call(scclust::sc_clustering, sc_call)

  class(out_matching) <- c("qm_matching", class(out_matching))
  out_matching
}
