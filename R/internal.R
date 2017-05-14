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

# Translate character indicators for different treatments to unit indices.
# get_target_indicators(c("A", "C"), factor(c("A", "B", "A", "C", "B")))
# > c(1, 3, 4)
get_target_indicators <- function(target,
                                  treatments) {
  stopifnot(is.character(target),
            !anyDuplicated(target),
            is.factor(treatments),
            all(target %in% levels(treatments)))

  target_indicators <- rep(FALSE, nlevels(treatments))
  names(target_indicators) <- levels(treatments)
  target_indicators[target] <- TRUE

  if (all(target_indicators)) {
    return(NULL)
  } else {
    return(.Call(qmc_get_target_indicators,
                 c(FALSE, target_indicators),
                 unclass(treatments)))
  }
}


internal_matching_weights <- function(treatments,
                                      matching,
                                      target = NULL) {
  stopifnot(is.factor(treatments),
            scclust::is.scclust(matching),
            length(matching) == length(treatments),
            is.null(target) || is.logical(target) || is.integer(target))
  .Call(qmc_matching_weights,
        unclass(treatments),
        nlevels(treatments),
        matching,
        target)
}
