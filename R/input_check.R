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

is.numeric_integer <- function(x) {
  is.numeric(x) && !any(is.nan(x)) && !any(is.infinite(x)) && all(x == as.integer(x))
}

new_error <- function(...) {
  stop(structure(list(message = paste0(...),
                      call = match.call(definition = sys.function(-2),
                                        call = sys.call(which = -2),
                                        expand.dots = TRUE,
                                        envir = sys.frame(-3))),
                 class = c("error", "condition")))
}

new_warning <- function(...) {
  warning(structure(list(message = paste0(...),
                         call = match.call(definition = sys.function(-2),
                                           call = sys.call(which = -2),
                                           expand.dots = TRUE,
                                           envir = sys.frame(-3))),
                    class = c("warning", "condition")))
}

check_length <- function(x,
                         req_length) {
  if (length(x) != req_length) {
    new_error("`", match.call()$x, "` is not of length `", match.call()$req_length, "`.")
  }
}

check_outcomes <- function(outcomes,
                           req_length = NULL) {
  if (!is.numeric(outcomes)) {
    new_error("`", match.call()$outcomes, "` is not numeric.")
  } else if (!is.null(req_length) && (length(outcomes) != req_length)) {
    new_error("`", match.call()$outcomes, "` is not of length `", match.call()$req_length, "`.")
  }
}

check_matching <- function(matching,
                           req_length = NULL) {
  if (!is.qm_matching(matching)) {
    new_error("`", match.call()$matching, "` is not a `qm_matching` object.")
  }
  if (!is.null(req_length) && (length(matching) != req_length)) {
    new_error("`", match.call()$matching, "` is not of length `", match.call()$req_length, "`.")
  }
}

coerce_labels <- function(labels) {
  if (!is.factor(labels) && !is.integer(labels)) {
    if (is.numeric(labels)) {
      if (is.numeric_integer(labels)) {
        labels <- as.integer(labels)
      } else {
        new_error("`", match.call()$labels, "` must be integer or factor.")
      }
    } else {
      new_warning("Coercing `", match.call()$labels, "` to factor.")
      labels <- as.factor(labels)
    }
  }
  labels
}

check_treatments <- function(treatments,
                             req_length = NULL) {
  if (!is.factor(treatments) && !is.integer(treatments)) {
    new_error("`", match.call()$treatments, "` must be integer or factor.")
  }
  if (is.factor(treatments)) {
    if (any(is.na(treatments))) {
      new_error("`", match.call()$treatments, "` may not contain NAs.")
    }
  } else if (is.integer(treatments)) {
    minNA <- min(treatments, na.rm = FALSE)
    if (is.na(minNA)) {
      new_error("`", match.call()$treatments, "` may not contain NAs.")
    }
    if (minNA < 0L) {
      new_error("`", match.call()$treatments, "` must not contain negtive entries.")
    }
  }
  if (!is.null(req_length) && (length(treatments) != req_length)) {
    new_error("`", match.call()$treatments, "` is not of length `", match.call()$req_length, "`.")
  }
}

check_against_treatments <- function(labels,
                                     treatments) {
  stopifnot(is.factor(treatments) || is.integer(treatments))
  if ((is.factor(treatments) && !all(as.character(labels) %in% levels(treatments))) ||
      (is.integer(treatments) && !all(as.integer(labels) %in% treatments))) {
    new_error("Some labels in `", match.call()$labels, "` are not in `", match.call()$treatments, "`.")
  }
}

check_indicators <- function(indicators,
                             req_length = NULL) {
  stopifnot(is.logical(indicators))
  if (any(is.na(indicators))) {
    new_error("`", match.call()$indicators, "` may not contain NAs.")
  }
  if (!is.null(req_length) && (length(indicators) != req_length)) {
    new_error("`", match.call()$indicators, "` is not of length `", match.call()$req_length, "`.")
  }
}
