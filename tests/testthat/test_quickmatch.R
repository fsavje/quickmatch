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
context("quickmatch")


test_quickmatch_against_Rscclust <- function(distances,
                                             treatments,
                                             treatment_constraints,
                                             total_size_constraint,
                                             subset,
                                             caliper,
                                             Rscc_type_constraints,
                                             Rscc_primary_data_points) {
  Rscc_cl <- Rscclust::nng_clustering_types(distance_object = distances,
                                            type_labels = treatments,
                                            type_size_constraints = Rscc_type_constraints,
                                            total_size_constraint = total_size_constraint,
                                            radius = if(is.null(caliper)) { caliper } else { caliper / 2.0 },
                                            primary_data_points = Rscc_primary_data_points)
  class(Rscc_cl) <- c("qm_matching", class(Rscc_cl))
  eval(bquote(expect_identical(quickmatch(distances,
                                          treatments,
                                          treatment_constraints,
                                          total_size_constraint,
                                          subset,
                                          caliper),
                               Rscc_cl)))
}


test_distances <- Rscclust::make_distances(matrix(1:200, ncol = 2))

test_treatments1 <- rep(1:2, 50)
test_treatments2 <- factor(rep(1:2, 50))
test_treatments3 <- rep(c("a", "b"), 50)
test_treatments4 <- rep(c(TRUE, FALSE), 50)

test_treat_constraint1 <- c("1" = 1L, "2" = 1L)
test_treat_constraint2 <- c("a" = 1L, "b" = 1L)
test_treat_constraint3 <- c("TRUE" = 1L, "FALSE" = 1L)
test_treat_constraint4 <- c("1" = 3L, "2" = 3L)

test_subset1 <- 1L
test_subset2 <- "2"
test_subset3 <- "a"
test_subset4 <- "FALSE"
test_subset5 <- c(rep(TRUE, 50), rep(FALSE, 50))


test_that("`quickmatch` returns correct output", {
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint1,
                                   NULL,
                                   NULL,
                                   NULL,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint1,
                                   NULL,
                                   NULL,
                                   NULL,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   test_treat_constraint2,
                                   NULL,
                                   NULL,
                                   NULL,
                                   test_treat_constraint2,
                                   NULL)
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  test_treat_constraint3,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  NULL))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   NULL,
                                   NULL,
                                   NULL,
                                   NULL,
                                   test_treat_constraint2,
                                   NULL)
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  NULL))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint4,
                                   NULL,
                                   NULL,
                                   11.0,
                                   test_treat_constraint4,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint4,
                                   NULL,
                                   NULL,
                                   NULL,
                                   test_treat_constraint4,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint1,
                                   10L,
                                   NULL,
                                   NULL,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint1,
                                   10L,
                                   NULL,
                                   NULL,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   test_treat_constraint2,
                                   10L,
                                   NULL,
                                   15.0,
                                   test_treat_constraint2,
                                   NULL)
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  test_treat_constraint3,
                                                  10L,
                                                  NULL,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  NULL))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   NULL,
                                   10L,
                                   NULL,
                                   NULL,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   NULL,
                                   10L,
                                   NULL,
                                   15.5,
                                   test_treat_constraint1,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   NULL,
                                   10L,
                                   NULL,
                                   NULL,
                                   test_treat_constraint2,
                                   NULL)
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  NULL,
                                                  10L,
                                                  NULL,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  NULL))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint4,
                                   10L,
                                   NULL,
                                   15.5,
                                   test_treat_constraint4,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint4,
                                   10L,
                                   NULL,
                                   NULL,
                                   test_treat_constraint4,
                                   NULL)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint1,
                                   NULL,
                                   test_subset2,
                                   NULL,
                                   test_treat_constraint1,
                                   test_treatments1 == 2L)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint1,
                                   NULL,
                                   test_subset1,
                                   3.5,
                                   test_treat_constraint1,
                                   test_treatments2 == "1")
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   test_treat_constraint2,
                                   NULL,
                                   test_subset3,
                                   NULL,
                                   test_treat_constraint2,
                                   test_treatments3 == "a")
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  test_treat_constraint3,
                                                  NULL,
                                                  test_subset4,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  !test_treatments4))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   NULL,
                                   NULL,
                                   test_subset1,
                                   NULL,
                                   test_treat_constraint1,
                                   test_treatments1 == 1L)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   NULL,
                                   NULL,
                                   test_subset2,
                                   NULL,
                                   test_treat_constraint1,
                                   test_treatments2 == "2")
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   NULL,
                                   NULL,
                                   test_subset3,
                                   3.5,
                                   test_treat_constraint2,
                                   test_treatments3 == "a")
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  NULL,
                                                  NULL,
                                                  test_subset4,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  !test_treatments4))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint4,
                                   NULL,
                                   test_subset2,
                                   NULL,
                                   test_treat_constraint4,
                                   test_treatments1 == 2L)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint4,
                                   NULL,
                                   test_subset1,
                                   NULL,
                                   test_treat_constraint4,
                                   test_treatments2 == "1")
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint1,
                                   10L,
                                   test_subset2,
                                   NULL,
                                   test_treat_constraint1,
                                   test_treatments1 == 2L)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint1,
                                   10L,
                                   test_subset1,
                                   NULL,
                                   test_treat_constraint1,
                                   test_treatments2 == "1")
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   test_treat_constraint2,
                                   10L,
                                   test_subset3,
                                   NULL,
                                   test_treat_constraint2,
                                   test_treatments3 == "a")
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  test_treat_constraint3,
                                                  10L,
                                                  test_subset4,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  !test_treatments4))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   NULL,
                                   10L,
                                   test_subset1,
                                   NULL,
                                   test_treat_constraint1,
                                   test_treatments1 == 1L)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   NULL,
                                   10L,
                                   test_subset2,
                                   NULL,
                                   test_treat_constraint1,
                                   test_treatments2 == "2")
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   NULL,
                                   10L,
                                   test_subset3,
                                   NULL,
                                   test_treat_constraint2,
                                   test_treatments3 == "a")
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  NULL,
                                                  10L,
                                                  test_subset4,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  !test_treatments4))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint4,
                                   10L,
                                   test_subset2,
                                   NULL,
                                   test_treat_constraint4,
                                   test_treatments1 == 2L)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint4,
                                   10L,
                                   test_subset1,
                                   NULL,
                                   test_treat_constraint4,
                                   test_treatments2 == "1")
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint1,
                                   NULL,
                                   test_subset5,
                                   NULL,
                                   test_treat_constraint1,
                                   test_subset5)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments2,
                                   test_treat_constraint1,
                                   NULL,
                                   test_subset5,
                                   3.5,
                                   test_treat_constraint1,
                                   test_subset5)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments3,
                                   test_treat_constraint2,
                                   NULL,
                                   test_subset5,
                                   NULL,
                                   test_treat_constraint2,
                                   test_subset5)
  expect_warning(test_quickmatch_against_Rscclust(test_distances,
                                                  test_treatments4,
                                                  test_treat_constraint3,
                                                  NULL,
                                                  test_subset5,
                                                  NULL,
                                                  test_treat_constraint3,
                                                  test_subset5))
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint4,
                                   NULL,
                                   test_subset5,
                                   12.5,
                                   test_treat_constraint4,
                                   test_subset5)
  test_quickmatch_against_Rscclust(test_distances,
                                   test_treatments1,
                                   test_treat_constraint1,
                                   10L,
                                   test_subset5,
                                   NULL,
                                   test_treat_constraint1,
                                   test_subset5)
})
