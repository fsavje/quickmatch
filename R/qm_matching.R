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

#' Constructor for qm_matching objects
#'
#' The \code{qm_matching} function constructs a \code{qm_matching} object from
#' existing matched group labels. The function does not derive matchings from
#' sets of data points; see \code{\link{quickmatch}} for that functionality.
#'
#' \code{qm_matching} objects are based on integer vectors, and it indexes
#' matched groups starting with zero. The \code{qm_matching} class inherits
#' from the \code{\link[scclust]{scclust}} class.
#'
#' @param group_labels
#'    a vector containing each unit's group label.
#' @param unassigned_labels
#'    labels that denote unassigned units. If \code{NULL}, \code{NA} values in
#'    \code{group_labels} are used to denote unassigned points.
#' @param ids
#'    IDs of the units. Should be a vector of the same length as
#'    \code{group_labels} or \code{NULL}. If \code{NULL}, the IDs are set to
#'    \code{1:length(group_labels)}.
#'
#' @return
#'    Returns a \code{qm_matching} object with the matching described by the
#'    provided labels.
#'
#' @examples
#' # 10 units in 3 matched groups
#' matches1 <- qm_matching(c("A", "A", "B", "C", "B",
#'                           "C", "C", "A", "B", "B"))
#'
#' # 8 units in 3 matched groups, 2 units unassigned
#' matches2 <- qm_matching(c(1, 1, 2, 3, 2,
#'                           NA, 3, 1, NA, 2))
#'
#' # Custom labels indicating unassiged units
#' matches3 <- qm_matching(c("A", "A", "B", "C", "NONE",
#'                           "C", "C", "NONE", "B", "B"),
#'                         unassigned_labels = "NONE")
#'
#' # Two different labels indicating unassiged units
#' matches4 <- qm_matching(c("A", "A", "B", "C", "NONE",
#'                           "C", "C", "0", "B", "B"),
#'                         unassigned_labels = c("NONE", "0"))
#'
#' # Custom unit IDs
#' matches5 <- qm_matching(c("A", "A", "B", "C", "B",
#'                           "C", "C", "A", "B", "B"),
#'                         ids = letters[1:10])
#'
#' @export
qm_matching <- function(group_labels,
                        unassigned_labels = NULL,
                        ids = NULL) {
  out_matching <- scclust::scclust(cluster_labels = group_labels,
                                   unassigned_labels = unassigned_labels,
                                   ids = ids)
  class(out_matching) <- c("qm_matching", class(out_matching))
  out_matching
}


#' Check qm_matching object
#'
#' \code{is.qm_matching} checks whether the provided object is a valid instance
#' of the \code{\link{qm_matching}} class.
#'
#' \code{is.qm_matching} does not check whether the matching itself is sensible
#' or whether it satisfies some set of constraints. See
#' \code{\link[scclust]{check_clustering}} for that functionality.
#'
#' @param x
#'    object to check.
#'
#' @return
#'    Returns \code{TRUE} if \code{x} is a valid \code{\link{qm_matching}}
#'    object, otherwise \code{FALSE}.
#'
#' @export
is.qm_matching <- function(x) {
  inherits(x, "qm_matching") && scclust::is.scclust(x)
}
