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
# get_subset_indicators(c("A", "C"), factor(c("A", "B", "A", "C", "B")))
# > c(1, 3, 4)
get_subset_indicators <- function(subset,
                                  treatments) {
  stopifnot(is.character(subset),
            !anyDuplicated(subset),
            is.factor(treatments),
            all(subset %in% levels(treatments)))

  subset_indicators <- rep(FALSE, nlevels(treatments))
  names(subset_indicators) <- levels(treatments)
  subset_indicators[subset] <- TRUE

  if (all(subset_indicators)) {
    return(NULL)
  } else {
    return(.Call(qmc_get_subset_indicators,
                 c(FALSE, subset_indicators),
                 unclass(treatments)))
  }
}
