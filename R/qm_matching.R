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


#' \code{qm_matching} objects
#'
#' Construct and check \code{qm_matching} objects.
#'
#' The \code{qm_matching} function constructs a \code{qm_matching} object
#' from existing matching labels. This is useful when one uses the estimators in
#' the package with a matching not constructed by the \code{\link{quickmatch}}
#' function.
#'
#' \code{is.qm_matching} checks whether the provided object
#' is a valid instance of the \code{qm_matching} class.
#' It does not check whether the matching it describes is sensible.
#' See \code{\link[Rscclust]{check_clustering_types}} for a function
#' that provides such checks.
#'
#' \code{qm_matching} objects are based on integer vectors and indexes
#' matched groups starting with zero. The \code{qm_matching} class
#' inherits from the \code{\link[Rscclust]{Rscc_clustering}} class in
#' \code{Rscclust}.
#'
#'
#' @param labels             vector with labels describing the matched groups.
#'
#' @param unassigned_labels  labels denoting unassigned data points.
#'                           \code{NA} values are always considered unassigned.
#'
#' @param ids                vector with IDs for the data points. If \code{NULL},
#'                           the IDs are set to \code{1:length(labels)}.
#'
#' @param obj                object to check.
#'
#' @return The \code{qm_matching} function returns a \code{qm_matching} object.
#'
#'         \code{is.qm_matching} returns \code{TRUE} if \code{obj} is valid,
#'         otherwise \code{FALSE}.
#'
#' @examples
#' # Ten units in three matched groups
#' qm_matching(c("A", "B", "B", "C", "B", "C", "A", "A", "C", "C"))
#'
#' # Label "999" denotes, in this example, units not assigned to a matched group
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
#' # Check whether constructed object is a qm_matching
#' is.qm_matching(qm_matching(c("A", "B", "B", "C")))
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


#' @rdname qm_matching
#' @export
is.qm_matching <- function(obj) {
  inherits(obj, "qm_matching") &&
    Rscclust::is.Rscc_clustering(obj)
}
