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


#' Construct \code{qm_matching} objects
#'
#' \code{qm_matching} constructs a \code{qm_matching} object from existing
#' matching labels. This is useful when one uses the estimators in
#' the package with a matching not constructed by the \code{\link{quickmatch}}
#' function.
#'
#' \code{qm_matching} inherits from \code{Rscc_clustering}.
#'
#' @param labels             vector with labels describing which matched group each unit
#'                           is assigned to.
#'
#' @param unassigned_labels  labels that denote unassigned data points.
#'                           \code{NA} values are always considered unassigned.
#'
#' @param ids                optional vector with IDs for the data points. If \code{NULL},
#'                           the IDs are set to \code{1:length(labels)}.
#'
#' @return Returns a \code{qm_matching} object.
#'
#' @examples
#' # Ten units in three matched groups
#' qm_matching(c("A", "B", "B", "C", "B", "C", "A", "A", "C", "C"))
#'
#' # Label "999" denotes units not assigned to a matched group
#' qm_matching(c("A", "999", "B", "C", "B", "999", "A", "A", "C", "C"),
#'             unassigned_labels = "999")
#'
#' # Equivalent to previous command
#' qm_matching(c("A", NA, "B", "C", "B", NA, "A", "A", "C", "C"))
#'
#' # Custom IDs
#' qm_matching(c("A", "B", "B", "C", "B", "C", "A", "A", "C", "C"),
#'             ids = letters[1:10])
#'
#' @export
qm_matching <- function(labels,
                        unassigned_labels = NULL,
                        ids = NULL) {
  labels <- Rscclust:::coerce_cluster_labels(labels, unassigned_labels)
  if (!is.null(ids)) {
    ids <- Rscclust:::coerce_character(ids, length(labels))
  }

  out_matching <- Rscclust::Rscc_clustering(cluster_labels = labels,
                                            unassigned_labels = NULL,
                                            ids = ids)

  class(out_matching) <- c("qm_matching", class(out_matching))
  out_matching
}


#' Check \code{qm_matching} object
#'
#' \code{is.qm_matching} checks whether the provided object
#' is a valid instance of the \code{\link{qm_matching}} class.
#' It does not check whether the matching it describes is sensible.
#'
#' @param obj  object to check.
#'
#' @return Returns \code{TRUE} if \code{obj} is valid,
#'         otherwise \code{FALSE}.
#'
#' @export
is.qm_matching <- function(obj) {
  inherits(obj, "qm_matching") &&
    Rscclust::is.Rscc_clustering(obj)
}
