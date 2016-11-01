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

get_all_treatment_conditions <- function(treatments) {
  stopifnot(is.factor(treatments) || is.integer(treatments))
  if (is.factor(treatments)) {
    out_conditions <- levels(treatments)
  } else if (is.integer(treatments)) {
    out_conditions <- sort(unique(treatments))
  }
  out_conditions
}

get_treatment_indicators <- function(targets,
                                     treatments) {
  stopifnot(is.factor(treatments) || is.integer(treatments))
  if (is.factor(treatments)) {
    stopifnot(all(as.character(targets) %in% levels(treatments)))
    out_indicators <- rep(FALSE, nlevels(treatments))
    names(out_indicators) <- levels(treatments)
    out_indicators[as.character(targets)] <- TRUE
    out_indicators <- c(FALSE, out_indicators)
  } else if (is.integer(treatments)) {
    max_label <- max(treatments)
    stopifnot(all(as.integer(targets) %in% 0L:max_label))
    out_indicators <- rep(FALSE, max_label + 1L)
    names(out_indicators) <- as.character(0L:max_label)
    out_indicators[as.character(as.integer(targets))] <- TRUE
  }
  out_indicators
}

get_distance_obs <- function(distance_object) {
  ncol(distance_object)
}
