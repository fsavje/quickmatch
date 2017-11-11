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

#' quickmatch: Quick Generalized Full Matching
#'
#' Provides functions for constructing near-optimal generalized full matchings.
#' Generalized full matching is an extension of the original full matching method
#' to situations with more intricate study designs. The package is made with
#' large data sets in mind and derives matchings more than an order of magnitude
#' quicker than other methods.
#'
#' See \code{\link{quickmatch}} for the main matching function.
#'
#' See the package's website for more information:
#' \url{https://github.com/fsavje/quickmatch}.
#'
#' Bug reports and suggestions are greatly appreciated. They are best reported
#' here: \url{https://github.com/fsavje/quickmatch/issues}.
#'
#' @references
#'    Sävje, Fredrik and Michael J. Higgins and Jasjeet S. Sekhon (2017),
#'    \sQuote{Generalized Full Matching}, arXiv 1703.03882.
#'    \url{https://arxiv.org/abs/1703.03882}
#'
#' @docType package
#' @name quickmatch-package
#'
#' @import distances
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please cite the `quickmatch` package as:")
  packageStartupMessage("   Savje, Fredrik, Michael J. Higgins and Jasjeet S. Sekhon (2017),")
  packageStartupMessage("   \"Generalized Full Matching\", arXiv 1703.03882.")
  packageStartupMessage("   https://arxiv.org/abs/1703.03882")
}

#' @useDynLib quickmatch, .registration = TRUE
.onUnload <- function (libpath) {
  library.dynam.unload("quickmatch", libpath)
}
