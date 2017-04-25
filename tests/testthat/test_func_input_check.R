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

library(quickmatch)
context("Input checking in exported functions")


# ==============================================================================
# Shared objects
# ==============================================================================

sound_covariates <- matrix(c(1, 4, 3, 2, 45, 6, 3, 2, 6, 5,
                             34, 2, 4, 6, 4, 6, 4, 2, 7, 8,
                             5, 6, 4, 6, 4, 25, 2, 1, 7, 5,
                             4, 8, 7, 6, 78, 6, 4, 6, 5, 6), nrow = 20)
unsound_covariates <- letters[1:20]
sound_distances <- distances(sound_covariates)
unsound_distances <- letters[1:20]
sound_treatments <- rep(1:2, 10)
unsound_treatments <- dist(1:10)
sound_treatment_constraints <- c("1" = 2L, "2" = 0L)
unsound_treatment_constraints <- c(2L, 0L)
sound_size_constraint <- 3L
unsound_size_constraint <- 100L
sound_subset <- NULL
unsound_subset <- "a"
sound_caliper <- NULL
unsound_caliper <- "a"
sound_group_labels <- letters[1:10]
unsound_group_labels <- dist(1:10)
sound_unassigned_labels <- c("a", "c")
unsound_unassigned_labels <- dist(1:10)
sound_ids <- letters[1:10]
unsound_ids <- letters[1:5]
sound_outcomes <- as.numeric(1:20)
unsound_outcomes <- letters[1:20]
sound_matching <- quickmatch(sound_distances, sound_treatments)
unsound_matching <- dist(1:10)


# ==============================================================================
# qm_matching
# ==============================================================================

t_qm_matching <- function(group_labels = sound_group_labels,
                          unassigned_labels = sound_unassigned_labels,
                          ids = sound_ids) {
  qm_matching(group_labels = group_labels,
              unassigned_labels = unassigned_labels,
              ids = ids)
}

test_that("`qm_matching` checks input.", {
  expect_silent(t_qm_matching())
  expect_error(t_qm_matching(group_labels = unsound_group_labels))
  expect_error(t_qm_matching(unassigned_labels = unsound_unassigned_labels))
  expect_error(t_qm_matching(ids = unsound_ids))
})


# ==============================================================================
# quickmatch
# ==============================================================================

t_quickmatch <- function(distances = sound_distances,
                         treatments = sound_treatments,
                         treatment_constraints = sound_treatment_constraints,
                         size_constraint = sound_size_constraint,
                         subset = sound_subset,
                         caliper = sound_caliper,
                         ...) {
  quickmatch(distances = distances,
             treatments = treatments,
             treatment_constraints = treatment_constraints,
             size_constraint = size_constraint,
             subset = subset,
             caliper = caliper,
             ...)
}

test_that("`quickmatch` checks input.", {
  expect_silent(t_quickmatch())
  expect_silent(t_quickmatch(caliper = 10))
  expect_error(t_quickmatch(distances = unsound_distances))
  expect_error(t_quickmatch(treatments = unsound_treatments))
  expect_error(t_quickmatch(treatment_constraints = unsound_treatment_constraints))
  expect_error(t_quickmatch(size_constraint = unsound_size_constraint))
  expect_error(t_quickmatch(subset = unsound_subset))
  expect_error(t_quickmatch(caliper = unsound_caliper))

  expect_silent(t_quickmatch(distances = sound_covariates))
  expect_error(t_quickmatch(distances = unsound_covariates))
  expect_silent(t_quickmatch(distances = sound_covariates, normalize = "mahalanobize"))
  expect_error(t_quickmatch(distances = sound_covariates, normalize = "non-exist"))
  expect_silent(t_quickmatch(treatment_constraints = NULL))
  expect_error(t_quickmatch(primary_data_points = 1:20))

  expect_silent(t_quickmatch(seed_radius = 10))
  expect_silent(t_quickmatch(primary_unassigned_method = "ignore"))
  expect_error(t_quickmatch(primary_unassigned_method = "non-exist"))
  expect_silent(t_quickmatch(secondary_unassigned_method = "ignore"))
  expect_error(t_quickmatch(secondary_unassigned_method = "non-exist"))
  expect_silent(t_quickmatch(primary_radius = "no_radius"))
  expect_error(t_quickmatch(primary_radius = "non-exist"))
  expect_silent(t_quickmatch(secondary_radius = "no_radius"))
  expect_error(t_quickmatch(secondary_radius = "non-exist"))

  expect_warning(t_quickmatch(caliper = 10, primary_unassigned_method = "any_neighbor"),
                 regexp = "Caliper might perform poorly when `primary_unassigned_method`==\"closest_seed\".")
  expect_warning(t_quickmatch(caliper = 10, secondary_unassigned_method = "closest_assigned"),
                 regexp = "Caliper is not properly enforced when `secondary_unassigned_method`==\"closest_assigned\".")
  expect_warning(t_quickmatch(caliper = 10, primary_radius = "no_radius"),
                 regexp = "Caliper is not properly enforced unless `primary_radius`==\"seed_radius\".")
  expect_warning(t_quickmatch(caliper = 10, secondary_radius = "no_radius"),
                 regexp = "Caliper is not properly enforced unless `secondary_radius`==\"seed_radius\".")
  expect_warning(t_quickmatch(caliper = 10, seed_radius = 10),
                 regexp = "`caliper` is ignored when `seed_radius` is specified.")
})


# ==============================================================================
# quickmatch
# ==============================================================================

t_regression_estimator <- function(outcomes = sound_outcomes,
                                   treatments = sound_treatments,
                                   matching = sound_matching,
                                   covariates = sound_covariates,
                                   subset = sound_subset) {
  regression_estimator(outcomes = outcomes,
                       treatments = treatments,
                       matching = matching,
                       covariates = covariates,
                       subset = subset)
}

test_that("`regression_estimator` checks input.", {
  expect_silent(t_regression_estimator())
  expect_silent(t_regression_estimator(covariates = NULL))
  expect_silent(t_regression_estimator(subset = "1"))
  expect_silent(t_regression_estimator(subset = rep(c(TRUE, FALSE), each = 10)))
  expect_warning(t_regression_estimator(treatments = rep(1:2, each = 10), subset = "1"))
  expect_error(t_regression_estimator(outcomes = unsound_outcomes))
  expect_error(t_regression_estimator(treatments = unsound_treatments))
  expect_error(t_regression_estimator(matching = unsound_matching))
  expect_error(t_regression_estimator(covariates = unsound_covariates))
  expect_error(t_regression_estimator(subset = unsound_subset))
})
