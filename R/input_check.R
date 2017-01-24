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


# ==============================================================================
# Ensure functions
# ==============================================================================

# Ensure that all `label_indicators` are represented in `treatments`
ensure_treatment_label_indicators <- function(label_indicators,
                                              treatments) {
  stopifnot(is.vector(treatments))
  if (!is.vector(label_indicators)) {
    Rscclust:::new_error("`", match.call()$label_indicators, "` must be vector.")
  }
  non_exist <- !(as.character(label_indicators) %in% as.character(treatments))
  if (any(non_exist)) {
    Rscclust:::new_error("`",
                         match.call()$label_indicators,
                         "` contains treatment labels without assigned units: ",
                         paste0(paste0("\"", as.character(label_indicators)[non_exist], "\""), collapse = ", "),
                         ".")
  }
}

# Using caliper with "closest_assigned" will not yield the result
# the user expect. Let's warn him or her.
ensure_sane_caliper <- function(caliper,
                                unassigned_method) {
  if (!is.null(caliper) && !is.null(unassigned_method)) {
    unassigned_method <- Rscclust:::coerce_args(unassigned_method,
                                                c("ignore",
                                                  "any_neighbor",
                                                  "closest_assigned",
                                                  "closest_seed"))
    if (unassigned_method != "closest_seed") {
      Rscclust:::new_warning("Non-NULL `caliper` with `unassigned_method` = \"", unassigned_method, "\".")
    }
  }
}


# ==============================================================================
# Coerce functions
# ==============================================================================

# Coerce `caliper` to NULL or a scalar, positive, non-na, numeric
coerce_caliper <- function(caliper) {
  if (!is.null(caliper)) {
    if (!is.numeric(caliper)) {
      Rscclust:::new_error("`", match.call()$caliper, "` must be numeric or `NULL`.")
    }
    if (length(caliper) != 1L) {
      Rscclust:::new_error("`", match.call()$caliper, "` must be scalar.")
    }
    if (is.na(caliper)) {
      Rscclust:::new_error("`", match.call()$caliper, "` may not be NA.")
    }
    if (caliper <= 0.0) {
      Rscclust:::new_error("`", match.call()$caliper, "` must be positive or `NULL`.")
    }
    caliper <- as.numeric(caliper) / 2.0
  }
  caliper
}


# Coerce `x` to double
coerce_double <- function(x,
                          req_length = NULL) {
  if (!is.double(x)) {
    if (is.numeric(x)) {
      x <- as.double(x)
    } else {
      Rscclust:::new_error("`", match.call()$x, "` is not numeric.")
    }
  }
  if (!is.null(req_length) && (length(x) != req_length)) {
    Rscclust:::new_error("`", match.call()$x, "` is not of length `", match.call()$req_length, "`.")
  }
  x
}
