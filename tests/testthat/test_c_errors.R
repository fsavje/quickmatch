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
context("Input checking in C code")


# ==============================================================================
# qmc_potential_outcomes.c
# ==============================================================================

c_potential_outcomes <- function(outcomes = as.numeric(1:20),
                                 treatments = rep(1:4, 5L),
                                 matching = qm_matching(c(rep(0L, 10), rep(1L, 10))),
                                 estimands = c(FALSE, TRUE, TRUE, TRUE, TRUE),
                                 subset = NULL) {
  .Call("qmc_potential_outcomes",
        outcomes,
        unclass(treatments),
        matching,
        estimands,
        subset,
        PACKAGE = "quickmatch")
}

test_that("`qmc_potential_outcomes` checks input.", {
  expect_silent(c_potential_outcomes())
  expect_silent(c_potential_outcomes(subset = rep(TRUE, 20)))
  expect_silent(c_potential_outcomes(subset = rep(TRUE, 20)))
  expect_silent(c_potential_outcomes(matching = structure(c(rep(0L, 10), rep(1L, 10)),
                                                          cluster_count = 2L)))
  expect_error(c_potential_outcomes(outcomes = "a"),
               regexp = "`R_outcomes` must be numeric.")
  expect_error(c_potential_outcomes(treatments = letters[1:20]),
               regexp = "`R_treatments` must be integer.")
  expect_error(c_potential_outcomes(treatments = rep(1:4, 4L)),
               regexp = "`R_treatments` and `R_outcomes` must be same length.")
  expect_error(c_potential_outcomes(matching = letters[1:20]),
               regexp = "`R_matching` must be integer.")
  expect_error(c_potential_outcomes(matching = qm_matching(c(rep(0L, 9), rep(1L, 9)))),
               regexp = "`R_matching` and `R_outcomes` must be same length.")
  expect_error(c_potential_outcomes(matching = c(rep(0L, 10), rep(1L, 10))),
               regexp = "`R_matching` is not valid `Rscc_clustering` object.")
  expect_error(c_potential_outcomes(matching = structure(c(rep(0L, 10), rep(1L, 10)),
                                                         cluster_count = 0L)),
               regexp = "`R_matching` is empty.")
  expect_error(c_potential_outcomes(estimands = 1:5),
               regexp = "`R_estimands` must be logical.")
  expect_error(c_potential_outcomes(subset = rep("a", 20)),
               regexp = "`R_subset` must be logical or NULL.")
  expect_error(c_potential_outcomes(subset = rep(TRUE, 15)),
               regexp = "`R_subset` and `R_outcomes` must be same length.")
  expect_error(c_potential_outcomes(matching = structure(c(rep(-1L, 10), rep(1L, 10)),
                                                         cluster_count = 2L)),
               regexp = "Matching out of bounds.")
  expect_error(c_potential_outcomes(matching = structure(c(rep(0L, 10), rep(2L, 10)),
                                                         cluster_count = 2L)),
               regexp = "Matching out of bounds.")
  expect_error(c_potential_outcomes(treatments = rep(2:5, 5L)),
               regexp = "Treatment out of bounds.")
  expect_error(c_potential_outcomes(treatments = rep(-1:2, 5L)),
               regexp = "Treatment out of bounds.")
})


# ==============================================================================
# qmc_potential_outcomes.c
# ==============================================================================

c_translate_targets <- function(targets = c(TRUE, FALSE, TRUE),
                                treatments = c(0L, 0L, 1L, 2L, 1L, 0L)) {
  .Call("qmc_translate_targets",
        targets,
        treatments,
        PACKAGE = "quickmatch")
}

test_that("`qmc_translate_targets` checks input.", {
  expect_silent(c_translate_targets())
  expect_error(c_translate_targets(targets = letters[1:3]),
               regexp = "`R_targets` must be logical.")
  expect_error(c_translate_targets(treatments = letters[1:6]),
               regexp = "`R_treatments` must be integer.")
  expect_error(c_translate_targets(treatments = c(0L, 0L, -1L, 2L, 1L, 0L)),
               regexp = "Treatment out of bounds.")
  expect_error(c_translate_targets(treatments = c(0L, 0L, 1L, 3L, 1L, 0L)),
               regexp = "Treatment out of bounds.")
})

