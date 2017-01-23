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

library(quickmatch)
context("Input checking in exported functions")


# ==============================================================================
# Shared objects
# ==============================================================================

sound_outcomes <- sqrt(1:20)
unsound_outcomes <- letters[1:20]
sound_treatments <- rep(1:2, 10)
unsound_treatments <- dist(1:10)
sound_matching <- qm_matching(c(rep(1L, 10), rep(2L, 10)))
unsound_matching <- c(rep("a", 10), rep("b", 10))
sound_treatment_labels <- c(1L, 2L)
unsound_treatment_labels <- c("a", "b")
sound_subset <- NULL
unsound_subset <- "a"
sound_bool <- TRUE
unsound_bool <- "a"


# ==============================================================================
# potential_outcomes
# ==============================================================================

test_that("`potential_outcomes` checks input.", {
  expect_silent(potential_outcomes(outcomes = sound_outcomes,
                                   treatments = sound_treatments,
                                   matching = sound_matching,
                                   targets = sound_treatment_labels,
                                   subset = sound_subset))
  expect_error(potential_outcomes(outcomes = unsound_outcomes,
                                  treatments = sound_treatments,
                                  matching = sound_matching,
                                  targets = sound_treatment_labels,
                                  subset = sound_subset))
  expect_error(potential_outcomes(outcomes = sound_outcomes,
                                  treatments = unsound_treatments,
                                  matching = sound_matching,
                                  targets = sound_treatment_labels,
                                  subset = sound_subset))
  expect_error(potential_outcomes(outcomes = sound_outcomes,
                                  treatments = sound_treatments,
                                  matching = unsound_matching,
                                  targets = sound_treatment_labels,
                                  subset = sound_subset))
  expect_error(potential_outcomes(outcomes = sound_outcomes,
                                  treatments = sound_treatments,
                                  matching = sound_matching,
                                  targets = unsound_treatment_labels,
                                  subset = sound_subset))
  expect_error(potential_outcomes(outcomes = sound_outcomes,
                                  treatments = sound_treatments,
                                  matching = sound_matching,
                                  targets = sound_treatment_labels,
                                  subset = unsound_subset))
})


# ==============================================================================
# treatment_effects
# ==============================================================================

test_that("`treatment_effects` checks input.", {
  expect_silent(treatment_effects(outcomes = sound_outcomes,
                                  treatments = sound_treatments,
                                  matching = sound_matching,
                                  contrasts = sound_treatment_labels,
                                  subset = sound_subset,
                                  drop = sound_bool))
  expect_error(treatment_effects(outcomes = unsound_outcomes,
                                 treatments = sound_treatments,
                                 matching = sound_matching,
                                 contrasts = sound_treatment_labels,
                                 subset = sound_subset,
                                 drop = sound_bool))
  expect_error(treatment_effects(outcomes = sound_outcomes,
                                 treatments = unsound_treatments,
                                 matching = sound_matching,
                                 contrasts = sound_treatment_labels,
                                 subset = sound_subset,
                                 drop = sound_bool))
  expect_error(treatment_effects(outcomes = sound_outcomes,
                                 treatments = sound_treatments,
                                 matching = unsound_matching,
                                 contrasts = sound_treatment_labels,
                                 subset = sound_subset,
                                 drop = sound_bool))
  expect_error(treatment_effects(outcomes = sound_outcomes,
                                 treatments = sound_treatments,
                                 matching = sound_matching,
                                 contrasts = unsound_treatment_labels,
                                 subset = sound_subset,
                                 drop = sound_bool))
  expect_error(treatment_effects(outcomes = sound_outcomes,
                                 treatments = sound_treatments,
                                 matching = sound_matching,
                                 contrasts = sound_treatment_labels,
                                 subset = unsound_subset,
                                 drop = sound_bool))
  expect_error(treatment_effects(outcomes = sound_outcomes,
                                 treatments = sound_treatments,
                                 matching = sound_matching,
                                 contrasts = sound_treatment_labels,
                                 subset = sound_subset,
                                 drop = unsound_bool))
})


# ==============================================================================
# qm_matching
# ==============================================================================

sound_labels <- 1:10
unsound_labels <- dist(1:10)
sound_unassigned_labels <- c(1L, 3L)
unsound_unassigned_labels <- c(1L, "a")
sound_ids <- letters[1:10]
unsound_ids <- letters[1:5]

test_that("`qm_matching` checks input.", {
  expect_silent(qm_matching(labels = sound_labels,
                            unassigned_labels = sound_unassigned_labels,
                            ids = sound_ids))
  expect_error(qm_matching(labels = unsound_labels,
                           unassigned_labels = sound_unassigned_labels,
                           ids = sound_ids))
  expect_error(qm_matching(labels = sound_labels,
                           unassigned_labels = unsound_unassigned_labels,
                           ids = sound_ids))
  expect_error(qm_matching(labels = sound_labels,
                           unassigned_labels = sound_unassigned_labels,
                           ids = unsound_ids))
})


# ==============================================================================
# quickmatch
# ==============================================================================

sound_distance_object <- Rscclust::make_distances(matrix(c(1, 4, 3, 2, 45, 6, 3, 2, 6, 5,
                                                           34, 2, 4, 6, 4, 6, 4, 2, 7, 8,
                                                           5, 6, 4, 6, 4, 25, 2, 1, 7, 5,
                                                           4, 8, 7, 6, 78, 6, 4, 6, 5, 6), nrow = 20))
unsound_distance_object <- letters[1:20]
sound_caliper <- NULL
unsound_caliper <- "a"
sound_treatment_constraints <- c("1" = 1L, "2" = 1L)
unsound_treatment_constraints <- c(1L, 1L)
sound_total_size_constraint <- 3L
unsound_total_size_constraint <- 100L

test_that("`quickmatch` checks input.", {
  expect_silent(quickmatch(distances = sound_distance_object,
                           treatments = sound_treatments,
                           treatment_constraints = sound_treatment_constraints,
                           total_size_constraint = sound_total_size_constraint,
                           caliper = sound_caliper,
                           subset = sound_subset))
  expect_error(quickmatch(distances = unsound_distance_object,
                          treatments = sound_treatments,
                          treatment_constraints = sound_treatment_constraints,
                          total_size_constraint = sound_total_size_constraint,
                          caliper = sound_caliper,
                          subset = sound_subset))
  expect_error(quickmatch(distances = sound_distance_object,
                          treatments = unsound_treatments,
                          treatment_constraints = sound_treatment_constraints,
                          total_size_constraint = sound_total_size_constraint,
                          caliper = sound_caliper,
                          subset = sound_subset))
  expect_error(quickmatch(distances = sound_distance_object,
                          treatments = sound_treatments,
                          treatment_constraints = unsound_treatment_constraints,
                          total_size_constraint = sound_total_size_constraint,
                          caliper = sound_caliper,
                          subset = sound_subset))
  expect_error(quickmatch(distances = sound_distance_object,
                          treatments = sound_treatments,
                          treatment_constraints = sound_treatment_constraints,
                          total_size_constraint = unsound_total_size_constraint,
                          caliper = sound_caliper,
                          subset = sound_subset))
  expect_error(quickmatch(distances = sound_distance_object,
                          treatments = sound_treatments,
                          treatment_constraints = sound_treatment_constraints,
                          total_size_constraint = sound_total_size_constraint,
                          caliper = unsound_caliper,
                          subset = sound_subset))
  expect_error(quickmatch(distances = sound_distance_object,
                          treatments = sound_treatments,
                          treatment_constraints = sound_treatment_constraints,
                          total_size_constraint = sound_total_size_constraint,
                          caliper = sound_caliper,
                          subset = unsound_subset))
  expect_warning(quickmatch(distances = sound_distance_object,
                            treatments = sound_treatments,
                            treatment_constraints = sound_treatment_constraints,
                            total_size_constraint = sound_total_size_constraint,
                            caliper = 5.0,
                            subset = sound_subset,
                            primary_unassigned_method = "closest_assigned"))
})
