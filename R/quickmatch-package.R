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


#' quickmatch: Fast Matching in Large Data Sets with Complicated Constraints
#'
#' \code{quickmatch} provides matching functions for constructing generalized full matchings
#' in large samples. It also provides computationally efficient estimators for
#' treatment effects and potential outcomes in matched groups. See
#' \code{\link{quickmatch}} for the core matching function. See
#' \code{\link{treatment_effects}} and \code{\link{potential_outcomes}}
#' for estimators.
#'
#' The package is still under development. Please use it with caution.
#'
#' More information and the latest version is found here:
#' \url{https://github.com/fsavje/quickmatch}.
#'
#' Bug reports and suggestions are greatly appreciated. They
#' are best reported here:
#' \url{https://github.com/fsavje/quickmatch/issues}.
#'
#' @docType package
#' @name quickmatch-package
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The `quickmatch` package is under development. Please use it with caution.")
  packageStartupMessage("Bug reports and suggestions are greatly appreciated: https://github.com/fsavje/quickmatch/issues")
}
