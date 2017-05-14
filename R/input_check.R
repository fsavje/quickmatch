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

# ==============================================================================
# Helper functions
# ==============================================================================

# Throw error
new_error <- function(...) {
  stop(structure(list(message = paste0(...),
                      call = match.call(definition = sys.function(-2),
                                        call = sys.call(which = -2),
                                        expand.dots = TRUE,
                                        envir = sys.frame(-3))),
                 class = c("error", "condition")))
}


# Throw warning
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
    !any(is.infinite(x)) &&
    all(is.na(x) | as.integer(x) == x)
}


# ==============================================================================
# Ensure functions
# ==============================================================================

# Ensure that `distances` is `distances` object
ensure_distances <- function(distances) {
  if (!distances::is.distances(distances)) {
    new_error("`", match.call()$distances, "` is not a `distances` object.")
  }
}


# Ensure that `matching` is a `scclust` object
ensure_matching <- function(matching,
                            req_length = NULL) {
  if (!scclust::is.scclust(matching)) {
    new_error("`", match.call()$matching, "` is not a valid matching object.")
  }
  if (!is.null(req_length) && (length(matching) != req_length)) {
    new_error("`", match.call()$matching, "` does not contain `", match.call()$req_length, "` units.")
  }
}


# Ensure `caliper` is NULL or a scalar, positive, non-na, numeric
ensure_caliper <- function(caliper) {
  if (!is.null(caliper)) {
    if (length(caliper) != 1L) {
      new_error("`", match.call()$caliper, "` must be scalar.")
    }
    if (is.na(caliper)) {
      new_error("`", match.call()$caliper, "` may not be NA.")
    }
    if (!is.numeric(caliper)) {
      new_error("`", match.call()$caliper, "` must be numeric or `NULL`.")
    }
    if (caliper <= 0.0) {
      new_error("`", match.call()$caliper, "` must be positive or `NULL`.")
    }
  }
}


# ==============================================================================
# Coerce functions
# ==============================================================================

# Coerce `x` to logical
coerce_logical <- function(x, req_length = NULL) {
  if (!is.vector(x)) {
    new_error("Do not know how to coerce `", match.call()$x, "` to logical.")
  }
  if (any(is.na(x))) {
    new_error("`", match.call()$x, "` may not be NA.")
  }
  if (!is.logical(x)) {
    x <- as.logical(x)
    if (any(is.na(x))) {
      new_error("Do not know how to coerce `", match.call()$x, "` to logical.")
    }
  }
  if (!is.null(req_length) && (length(x) != req_length)) {
    new_error("`", match.call()$x, "` is not of length `", match.call()$req_length, "`.")
  }
  x
}


# Coerce `covariates` to numeric matrix
coerce_covariates <- function(covariates, req_length) {
  if (!is.null(covariates)) {
    if (is.data.frame(covariates)) {
      covariates <- as.matrix(covariates)
    } else if (is.vector(covariates)) {
      covariates <- as.matrix(covariates, ncol = 1)
    }
    if (!is.matrix(covariates)) {
      new_error("`", match.call()$covariates, "` must be vector, matrix or data frame.")
    }
    covariates <- unname(covariates)
    if (!is.double(covariates)) {
      if (is.numeric(covariates)) {
        storage.mode(covariates) <- "double"
      } else {
        new_error("`", match.call()$covariates, "` is not numeric.")
      }
    }
    if (nrow(covariates) != req_length) {
      new_error("`", match.call()$covariates, "` is not of length `", match.call()$req_length, "`.")
    }
  }
  covariates
}


# Coerce `x` to double
coerce_double <- function(x, req_length = NULL) {
  if (!is.double(x)) {
    if (is.numeric(x)) {
      x <- as.double(x)
    } else {
      new_error("`", match.call()$x, "` is not numeric.")
    }
  }
  if (!is.null(req_length) && (length(x) != req_length)) {
    new_error("`", match.call()$x, "` is not of length `", match.call()$req_length, "`.")
  }
  x
}


# Coerce `size_constraint` to scalar, non-NA integer with default as `sum(type_constraints)`
coerce_size_constraint <- function(size_constraint,
                                   sum_treatment_constraints,
                                   num_data_points) {
  if (is.null(size_constraint)) {
    size_constraint <- sum_treatment_constraints
  }
  if (length(size_constraint) != 1L) {
    new_error("`", match.call()$size_constraint, "` must be scalar.")
  }
  if (!is.integer(size_constraint)) {
    if (is.numeric_integer(size_constraint)) {
      storage.mode(size_constraint) <- "integer"
    } else {
      new_error("`", match.call()$size_constraint, "` must be integer.")
    }
  }
  if (is.na(size_constraint)) {
    new_error("`", match.call()$size_constraint, "` may not be NA.")
  }
  if (size_constraint < 2L) {
    new_error("`", match.call()$size_constraint, "` must be greater or equal to two.")
  }
  if (size_constraint < sum_treatment_constraints) {
    new_error("`", match.call()$size_constraint, "` must be greater or equal to the sum of the treatment constraints.")
  }
  if (size_constraint > num_data_points) {
    new_error("`", match.call()$size_constraint, "` may not be great than the number of units.")
  }
  size_constraint
}


# Coerce `target` to indicator vector if character
coerce_target <- function(target,
                          treatments,
                          check_NA = TRUE) {
  stopifnot(is.factor(treatments))

  if (is.character(target)) {
    if (anyDuplicated(target)) {
      new_error("`", match.call()$target, "` may not contain duplicates.")
    }
    non_exist <- !(target %in% levels(treatments))
    if (any(non_exist)) {
      new_error("`", match.call()$target,
                "` contains unknown treatment labels: ",
                paste0(paste0("\"", target[non_exist], "\""), collapse = ", "),
                ".")
    }
    target <- get_target_indicators(target, treatments)
  } else if (is.logical(target)) {
    if (check_NA && anyNA(target)) {
      new_error("`", match.call()$target, "` may not contain NAs.")
    }
    if (check_NA && !any(target)) {
      new_error("`", match.call()$target, "` cannot be all `FALSE`.")
    }
    if (length(target) != length(treatments)) {
      new_error("`", match.call()$target, "` is not of the same length as `", match.call()$treatments, "`.")
    }
  } else if (!is.null(target)) {
    if (!is.integer(target)) {
      if (is.numeric_integer(target)) {
        storage.mode(target) <- "integer"
      } else {
        new_error("`", match.call()$target, "` must be integer, logical, character or NULL.")
      }
    }
    if (check_NA && anyDuplicated(target)) {
      new_error("`", match.call()$target, "` may not contain duplicates.")
    }
    if (check_NA && anyNA(target)) {
      new_error("`", match.call()$target, "` may not contain NAs.")
    }
    if (length(target) == 0) {
      new_error("`", match.call()$target, "` cannot be empty.")
    }
  }
  target
}


# Coerce `treatments` to factor
coerce_treatments <- function(treatments,
                              req_length = NULL,
                              check_NA = TRUE) {
  if (!is.factor(treatments)) {
    if (!is.vector(treatments)) {
      new_error("Do not know how to coerce `", match.call()$treatments, "` to factor.")
    }
    if (!is.integer(treatments) && !is.logical(treatments) && !is.character(treatments)) {
      new_warning("Coercing `", match.call()$treatments, "` to factor.")
    }
    treatments <- as.factor(treatments)
  }
  if (check_NA && any(is.na(treatments))) {
    new_error("`", match.call()$treatments, "` may not contain NAs.")
  }
  if (!is.null(req_length) && (length(treatments) != req_length)) {
    new_error("Length of `", match.call()$treatments, "` is incorrect.")
  }
  treatments
}


# Coerce `treatment_constraints` to valid constraints
coerce_treatment_constraints <- function(treatment_constraints,
                                         treatment_levels) {
  if (is.null(names(treatment_constraints))) {
    new_error("`", match.call()$treatment_constraints, "` must be named.")
  }
  if (anyDuplicated(names(treatment_constraints))) {
    new_error("`", match.call()$treatment_constraints, "` may not contain duplicate names.")
  }
  non_exist <- !(names(treatment_constraints) %in% treatment_levels)
  if (any(non_exist)) {
    new_error("`", match.call()$treatment_constraints,
              "` contains unknown treatment labels: ",
              paste0(paste0("\"", names(treatment_constraints)[non_exist], "\""), collapse = ", "),
              ".")
  }
  if (!is.integer(treatment_constraints)) {
    if (is.numeric_integer(treatment_constraints)) {
      storage.mode(treatment_constraints) <- "integer"
    } else {
      new_error("`", match.call()$treatment_constraints, "` must be integer.")
    }
  }
  if (any(is.na(treatment_constraints))) {
    new_error("`", match.call()$treatment_constraints, "` may not contain NAs.")
  }
  if (any(treatment_constraints < 0L)) {
    new_error("`", match.call()$treatment_constraints, "` must be non-negative.")
  }
  treatment_constraints
}
