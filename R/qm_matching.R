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


#' Construct a \code{qm_matching} object
#'
#' \code{qm_matching} constructs a \code{qm_matching} object from existing
#' matching labels.
#'
#' \code{qm_matching} inherits from \code{Rscc_clustering}.
#'
#' @param labels a vector containing each data point's label.
#'
#' @param unassigned_labels a vector containing labels that denote unassigned data points.
#'
#' @param ids optional IDs for the data points. Either a vector of the same length as
#'            \code{cluster_labels} or \code{NULL}. If \code{NULL}, the IDs are set to
#'            \code{1:length(labels)}.
#'
#' @return Returns a \code{qm_matching} object.
#'
#' @export
qm_matching <- function(labels,
                        unassigned_labels = NA,
                        ids = NULL) {
  labels <- coerce_labels(labels)
  if (!is.null(ids)) {
    check_length(ids, length(labels))
  }

  out_matching <- Rscclust::Rscc_clustering(cluster_labels = labels,
                                            unassigned_labels = unassigned_labels,
                                            ids = ids)

  class(out_matching) <- c("qm_matching", class(out_matching))
  out_matching
}


#' Check \code{qm_matching} object
#'
#' \code{check_qm_matching} checks whether the provided object
#' is a valid instance of the \code{qm_matching} class. It does
#' not whether the matching is sensible.
#'
#' @param obj  object to check.
#'
#' @return Returns \code{TRUE} if \code{obj} is a valid
#'         \code{qm_matching} object, otherwise \code{FALSE}.
#'
#' @export
is.qm_matching <- function(obj) {
  if (!inherits(obj, "qm_matching")) {
    return(FALSE)
  }
  if (!Rscclust::is.Rscc_clustering(obj)) {
    return(FALSE)
  }
  TRUE
}
