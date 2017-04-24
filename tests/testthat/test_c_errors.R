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
context("Input checking in C code")

# ==============================================================================
# utilities.c
# ==============================================================================

t_qmc_get_subset_indicators <- function(subset_indicators = c(TRUE, FALSE, TRUE),
                                        treatments = c(0L, 0L, 1L, 2L, 1L, 0L)) {
  .Call(qmc_get_subset_indicators,
        subset_indicators,
        treatments)
}

test_that("`qmc_get_subset_indicators` checks input.", {
  expect_silent(t_qmc_get_subset_indicators())
  expect_error(t_qmc_get_subset_indicators(subset_indicators = letters[1:3]),
               regexp = "`R_subset` must be logical.")
  expect_error(t_qmc_get_subset_indicators(treatments = letters[1:6]),
               regexp = "`R_treatments` must be integer.")
  expect_error(t_qmc_get_subset_indicators(treatments = c(0L, 0L, -1L, 2L, 1L, 0L)),
               regexp = "Treatment out of bounds.")
  expect_error(t_qmc_get_subset_indicators(treatments = c(0L, 0L, 1L, 3L, 1L, 0L)),
               regexp = "Treatment out of bounds.")
})
