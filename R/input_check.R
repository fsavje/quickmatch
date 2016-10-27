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

qm_check_numeric <- function(to_check,
                             req_length = NULL) {
  stopifnot(
    is.numeric(to_check),
    is.null(req_length) || (length(to_check) == req_length)
  )
}


qm_check_matching <- function(matching,
                              req_length = NULL) {
  stopifnot(
    inherits(matching, "Rscc_clustering"),
    is.integer(matching),
    "cluster_count" %in% names(attributes(matching)),
    as.integer(attr(matching, "cluster_count", exact = TRUE))[1] > 0L,
    is.null(req_length) || (length(matching) == req_length)
  )
}


qm_check_treatment <- function(treatments,
                               req_length = NULL) {
  stopifnot(
    is.factor(treatments) || is.integer(treatments),
    !is.factor(treatments) || all(!is.na(treatments)),
    !is.integer(treatments) || isTRUE(min(treatments, na.rm = FALSE) >= 0L),
    is.null(req_length) || (length(treatments) == req_length)
  )
}


qm_check_treatment_labels <- function(labels,
                                      treatments) {
  stopifnot(
    is.factor(treatments) || is.integer(treatments),
    !is.factor(treatments) || all(labels %in% levels(treatments)),
    !is.integer(treatments) || all(labels %in% treatments)
  )
}


qm_get_all_treatment_conditions <- function(treatments) {
  stopifnot(is.factor(treatments) || is.integer(treatments))
  if (is.factor(treatments)) {
    out_conditions <- levels(treatments)
  } else if (is.integer(treatments)) {
    out_conditions <- sort(unique(treatments))
  }
  out_conditions
}


qm_get_treatment_indicators <- function(targets,
                                        treatments) {

  stopifnot(is.factor(treatments) || is.integer(treatments))

  if (is.factor(treatments)) {
    out_indicators <- rep(FALSE, nlevels(treatments))
    names(out_indicators) <- levels(treatments)
    stopifnot(all(targets %in% names(out_indicators)))
    out_indicators[targets] <- TRUE
    out_indicators <- c(FALSE, out_indicators)
  } else if (is.integer(treatments)) {
    max_label <- max(treatments)
    out_indicators <- rep(FALSE, max_label + 1L)
    names(out_indicators) <- as.character(0L:max_label)
    stopifnot(all(as.character(targets) %in% names(out_indicators)))
    out_indicators[as.character(targets)] <- TRUE
  }

  out_indicators
}
