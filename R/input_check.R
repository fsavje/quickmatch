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
# Helper functions
# ==============================================================================

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


# Is `x` a numeric that can be coerced into integer without loss of information?
is.numeric_integer <- function(x) {
  is.numeric(x) &&
    !any(is.nan(x)) &&
    !any(is.infinite(x)) &
    all(x == as.integer(x))
}


# ==============================================================================
# Ensure functions
# ==============================================================================

# Ensure that `caliper` is a scalar, positive, non-na, numeric
ensure_caliper <- function(caliper) {
  if (!is.null(caliper)) {
    if (!is.numeric(caliper)) {
      new_error("`", match.call()$caliper, "` must be integer.")
    }
    if (length(caliper) != 1L) {
      new_error("`", match.call()$caliper, "` must be scalar.")
    }
    if (is.na(caliper)) {
      new_error("`", match.call()$caliper, "` may not be NA.")
    }
    if (caliper <= 0.0) {
      new_error("`", match.call()$caliper, "` must be positive or `NULL`.")
    }
  }
}


# Ensure that `counts` are non-NA, non-negative integers
ensure_counts <- function(counts,
                          req_length = NULL) {
  if (!is.integer(counts)) {
    new_error("`", match.call()$counts, "` must be integer.")
  }
  if (any(is.na(counts))) {
    new_error("`", match.call()$counts, "` may not contain NAs.")
  }
  if (any(counts < 0L)) {
    new_error("`", match.call()$counts, "` must not contain negtive entries.")
  }
  if (!is.null(req_length) && (length(counts) != req_length)) {
    new_error("`", match.call()$counts, "` is not of length `", match.call()$req_length, "`.")
  }
}


# Ensure that `distances` is `Rscc_distances` object
ensure_distances <- function(distances,
                             req_length = NULL) {
  if (!Rscclust::is.Rscc_distances(distances)) {
    new_error("`", match.call()$distances, "` is not a `Rscc_distances` object.")
  }
  if (!is.null(req_length) && (get_distance_obs(distances) != req_length)) {
    new_error("`", match.call()$distances, "` does not contain `", match.call()$req_length, "` data points.")
  }
}


# Ensure that `indicators` are non-NA logicals
ensure_indicators <- function(indicators,
                              req_length = NULL,
                              any_true = FALSE) {
  stopifnot(is.logical(indicators))
  if (any(is.na(indicators))) {
    new_error("`", match.call()$indicators, "` may not contain NAs.")
  }
  if (!is.null(req_length) && (length(indicators) != req_length)) {
    new_error("`", match.call()$indicators, "` is not of length `", match.call()$req_length, "`.")
  }
  if (any_true) {
    if (!any(indicators)) {
      new_error("`", match.call()$indicators, "` cannot be all `FALSE`.")
    }
  }
}


# Ensure `x` has length `req_length`
ensure_length <- function(x,
                          req_length) {
  if (length(x) != req_length) {
    new_error("`", match.call()$x, "` is not of length `", match.call()$req_length, "`.")
  }
}


# Ensure that `matching` is a `qm_matching` object
ensure_matching <- function(matching,
                            req_length = NULL) {
  if (!is.qm_matching(matching)) {
    new_error("`", match.call()$matching, "` is not a `qm_matching` object.")
  }
  if (!is.null(req_length) && (length(matching) != req_length)) {
    new_error("`", match.call()$matching, "` is not of length `", match.call()$req_length, "`.")
  }
}


# Ensure that `outcomes` is numeric
ensure_outcomes <- function(outcomes,
                            req_length = NULL) {
  if (!is.numeric(outcomes)) {
    new_error("`", match.call()$outcomes, "` is not numeric.")
  } else if (!is.null(req_length) && (length(outcomes) != req_length)) {
    new_error("`", match.call()$outcomes, "` is not of length `", match.call()$req_length, "`.")
  }
}


# Ensure that `treatments` is non-NA factor or non-NA, non-negative integer
ensure_treatments <- function(treatments,
                              req_length = NULL) {
  if (!is.factor(treatments) && !is.integer(treatments)) {
    new_error("`", match.call()$treatments, "` must be integer or factor.")
  }
  if (any(is.na(treatments))) {
    new_error("`", match.call()$treatments, "` may not contain NAs.")
  }
  if (is.integer(treatments) && any(treatments < 0L)) {
    new_error("`", match.call()$treatments, "` must not contain negtive entries.")
  }
  if (!is.null(req_length) && (length(treatments) != req_length)) {
    new_error("`", match.call()$treatments, "` is not of length `", match.call()$req_length, "`.")
  }
}


# Ensure that `constraints` are valid
ensure_treatment_constraints <- function(constraints) {
  if (is.null(names(constraints))) {
    new_error("`", match.call()$constraints, "` must be named.")
  }
  if (anyDuplicated(names(constraints))) {
    new_error("`", match.call()$constraints, "` may not contain duplicates.")
  }
  if (!is.integer(constraints)) {
    new_error("`", match.call()$constraints, "` must be integer.")
  }
  if (any(is.na(constraints))) {
    new_error("`", match.call()$constraints, "` may not contain NAs.")
  }
  if (any(constraints < 0L)) {
    new_error("`", match.call()$constraints, "` must not contain negtive entries.")
  }
}


# Ensure that all `labels` are represented in `treatments`
ensure_treatment_labels <- function(labels,
                                    treatments) {
  if (!all(as.character(labels) %in% as.character(treatments))) {
    missing_treatment <- as.character(labels)[!(as.character(labels) %in% as.character(treatments))][1]
    new_error("`", match.call()$labels, "` contains a treatment condition without assigned units: \"", missing_treatment, "\".")
  }
}


# ==============================================================================
# Coerce functions
# ==============================================================================

# Coerce `labels` to integer or factor
coerce_labels <- function(labels) {
  if (!is.factor(labels) && !is.integer(labels)) {
    if (is.numeric(labels)) {
      if (is.numeric_integer(labels)) {
        labels <- as.integer(labels)
      } else {
        new_error("`", match.call()$labels, "` must be integer or factor.")
      }
    } else if (is.character(labels)) {
      labels <- as.factor(labels)
    } else {
      new_warning("Coercing `", match.call()$labels, "` to factor.")
      labels <- as.factor(labels)
    }
  }
  labels
}


# Coerce `x` to integer
coerce_integer <- function(x) {
  if (!is.integer(x)) {
    if (is.numeric_integer(x)) {
      storage.mode(x) <- "integer"
    } else {
      new_error("`", match.call()$x, "` must be integer.")
    }
  }
  x
}
