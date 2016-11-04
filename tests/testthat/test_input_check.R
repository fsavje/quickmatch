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
context("Input checking in R code")


# ==============================================================================
# ensure_treatment_label_indicators
# ==============================================================================

t_ensure_treatment_label_indicators <- function(t_label_indicators = c("a", "c"),
                                                t_treatments = letters[1:10]) {
  ensure_treatment_label_indicators(t_label_indicators, t_treatments)
}

test_that("`ensure_treatment_label_indicators` checks input.", {
  expect_silent(t_ensure_treatment_label_indicators())
  expect_error(t_ensure_treatment_label_indicators(t_treatments = factor(letters[1:10])))
  expect_error(t_ensure_treatment_label_indicators(t_label_indicators = factor(c("a", "c"))),
               regexp = "`t_label_indicators` must be vector.")
  expect_error(t_ensure_treatment_label_indicators(t_label_indicators = c("a", "1")),
               regexp = "`t_label_indicators` contains treatment labels without assigned units: \"1\".")
  expect_error(t_ensure_treatment_label_indicators(t_label_indicators = c("1", "a", "z")),
               regexp = "`t_label_indicators` contains treatment labels without assigned units: \"1\", \"z\".")
})


# ==============================================================================
# coerce_double
# ==============================================================================

t_coerce_double <- function(t_x = 1:10,
                            t_req_length = NULL) {
  coerce_double(t_x, t_req_length)
}

test_that("`coerce_double` checks input.", {
  expect_silent(t_coerce_double())
  expect_silent(t_coerce_double(t_x = as.numeric(1:10)))
  expect_silent(t_coerce_double(t_req_length = 10))
  expect_error(t_coerce_double(t_x = letters[1:10]),
               regexp = "`t_x` is not numeric.")
  expect_error(t_coerce_double(t_req_length = 8),
               regexp = "`t_x` is not of length `t_req_length`.")
})

test_that("`coerce_double` coerces correctly.", {
  expect_identical(t_coerce_double(), as.numeric(1:10))
  expect_identical(t_coerce_double(t_x = as.numeric(1:10)), as.numeric(1:10))
  expect_identical(t_coerce_double(t_req_length = 10), as.numeric(1:10))
})
