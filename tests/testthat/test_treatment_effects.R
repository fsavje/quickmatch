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
context("treatment_effects")


te_matrix <- function(pos) {
  num_pos <- length(pos)
  out_mat <- matrix(NA, ncol = num_pos, nrow = num_pos)
  rownames(out_mat) <- names(pos)
  colnames(out_mat) <- names(pos)
  for (i_r in 1:num_pos) {
    for (i_c in 1:num_pos) {
      out_mat[i_r, i_c] <- pos[i_r] - pos[i_c]
    }
  }
  out_mat
}

te_diff <- function(pos) {
  stopifnot(length(pos) == 2L)
  out_diff <- pos[1] - pos[2]
  names(out_diff) <- paste0(names(pos)[1], "-", names(pos)[2])
  out_diff
}


test_outcome <- as.numeric(1:100)
test_treatment1 <- c("a", "a", "b", "a", "a", "a", "b", "b", "a", "a", "a", "b", "a", "b", "a", "a", "a", "b", "a", "b", "a", "b", "a", "b", "b", "a", "b", "a", "b", "a", "a", "a", "a", "b", "a", "b", "a", "a", "b", "a", "a", "b", "a", "b", "a", "b", "b", "b", "a", "b", "a", "a", "b", "a", "b", "a", "a", "b", "b", "a", "b", "a", "a", "b", "b", "b", "a", "a", "b", "b", "a", "b", "a", "a", "b", "b", "b", "b", "a", "b", "a", "a", "b", "b", "b", "a", "b", "a", "b", "a", "b", "b", "b", "a", "b", "b", "a", "b", "b", "b")
test_treatment2 <- c("a", "c", "d", "a", "b", "c", "d", "c", "d", "b", "a", "d", "a", "d", "c", "d", "d", "d", "b", "c", "c", "c", "d", "c", "a", "b", "b", "d", "c", "d", "b", "b", "a", "a", "d", "d", "c", "a", "b", "a", "b", "a", "a", "c", "b", "d", "a", "d", "b", "a", "c", "d", "c", "a", "d", "b", "c", "a", "d", "a", "d", "a", "b", "c", "c", "c", "b", "b", "a", "c", "b", "c", "d", "c", "b", "b", "c", "d", "b", "a", "a", "d", "c", "b", "c", "b", "c", "d", "d", "b", "d", "a", "b", "a", "a", "a", "c", "a", "b", "b")
test_matching <- qm_matching(c(rep(0, 25), rep(1, 25), rep(2, 25), rep(3, 25)))
test_subset1 <- c("a")
test_subset2 <- c(rep(TRUE, 50), rep(FALSE, 50))

test_that("potential_outcomes produces correct output.", {
  pos1 <- potential_outcomes(outcomes = test_outcome,
                             treatments = test_treatment1,
                             matching = test_matching,
                             estimands = NULL,
                             subset = NULL)
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment1,
                                 matching = test_matching,
                                 contrasts = NULL,
                                 subset = NULL,
                                 drop = TRUE),
               te_matrix(pos1))
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment1,
                                 matching = test_matching,
                                 contrasts = NULL,
                                 subset = NULL,
                                 drop = FALSE),
               te_matrix(pos1))

  pos2 <- potential_outcomes(outcomes = test_outcome,
                             treatments = test_treatment2,
                             matching = test_matching,
                             estimands = NULL,
                             subset = NULL)
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = NULL,
                                 subset = NULL,
                                 drop = TRUE),
               te_matrix(pos2))
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = NULL,
                                 subset = NULL,
                                 drop = FALSE),
               te_matrix(pos2))

  pos3 <- potential_outcomes(outcomes = test_outcome,
                             treatments = test_treatment2,
                             matching = test_matching,
                             estimands = c("a", "c"),
                             subset = NULL)
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = c("a", "c"),
                                 subset = NULL,
                                 drop = TRUE),
               te_diff(pos3))
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = c("a", "c"),
                                 subset = NULL,
                                 drop = FALSE),
               te_matrix(pos3))

  pos4 <- potential_outcomes(outcomes = test_outcome,
                             treatments = test_treatment2,
                             matching = test_matching,
                             estimands = NULL,
                             subset = test_subset1)
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = NULL,
                                 subset = test_subset1,
                                 drop = TRUE),
               te_matrix(pos4))

  pos5 <- potential_outcomes(outcomes = test_outcome,
                             treatments = test_treatment2,
                             matching = test_matching,
                             estimands = NULL,
                             subset = test_subset2)
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = NULL,
                                 subset = test_subset2,
                                 drop = TRUE),
               te_matrix(pos5))

  pos6 <- potential_outcomes(outcomes = test_outcome,
                             treatments = test_treatment2,
                             matching = test_matching,
                             estimands = c("a", "c", "d"),
                             subset = test_subset1)
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = c("a", "c", "d"),
                                 subset = test_subset1,
                                 drop = TRUE),
               te_matrix(pos6))

  pos7 <- potential_outcomes(outcomes = test_outcome,
                             treatments = test_treatment2,
                             matching = test_matching,
                             estimands = c("a", "b"),
                             subset = test_subset2)
  expect_equal(treatment_effects(outcomes = test_outcome,
                                 treatments = test_treatment2,
                                 matching = test_matching,
                                 contrasts = c("a", "b"),
                                 subset = test_subset2,
                                 drop = TRUE),
               te_diff(pos7))
})
