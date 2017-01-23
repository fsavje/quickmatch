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
context("Internal functions")


# ==============================================================================
# get_all_treatment_conditions
# ==============================================================================

test_that("`get_all_treatment_conditions` checks input.", {
  expect_silent(get_all_treatment_conditions(factor(rep(letters[1:5], 10))))
  expect_silent(get_all_treatment_conditions(rep(1:5, 10)))
  expect_silent(get_all_treatment_conditions(rep(5:1, 10)))
  expect_error(get_all_treatment_conditions("a"))
})

test_that("`get_all_treatment_conditions` returns correct output.", {
  expect_identical(get_all_treatment_conditions(factor(rep(letters[1:5], 10))),
                   letters[1:5])
  expect_identical(get_all_treatment_conditions(rep(1:5, 10)),
                   1:5)
  expect_identical(get_all_treatment_conditions(rep(5:1, 10)),
                   1:5)
})


# ==============================================================================
# translate_targets
# ==============================================================================

test_that("`translate_targets` checks input.", {
  expect_silent(translate_targets(c(TRUE, FALSE, TRUE),
                                  c(0L, 0L, 1L, 2L, 1L, 0L)))
  expect_silent(translate_targets(c(FALSE, TRUE, FALSE, TRUE),
                                  factor(c(0L, 0L, 1L, 2L, 1L, 0L))))
  expect_error(translate_targets(c("TRUE", "FALSE", "TRUE"),
                                 c(0L, 0L, 1L, 2L, 1L, 0L)))
  expect_error(translate_targets(c(TRUE, FALSE, TRUE),
                                 c("0L", "0L", "1L", "2L", "1L", "0L")))
})

test_that("`translate_targets` returns correct output.", {
  expect_identical(translate_targets(c(TRUE, FALSE, TRUE),
                                     c(0L, 0L, 1L, 2L, 1L, 0L)),
                   c(1L, 2L, 4L, 6L))
  expect_identical(translate_targets(c(FALSE, TRUE, FALSE, TRUE),
                                     factor(c(0L, 0L, 1L, 2L, 1L, 0L))),
                   c(1L, 2L, 4L, 6L))
})
