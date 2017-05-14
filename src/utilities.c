/* =============================================================================
 * quickmatch -- Quick Generalized Full Matching
 * https://github.com/fsavje/quickmatch
 *
 * Copyright (C) 2017  Fredrik Savje -- http://fredriksavje.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/
 * ========================================================================== */

#include "utilities.h"
#include <stddef.h>
#include <R.h>
#include <Rinternals.h>
#include "error.h"


// =============================================================================
// External function implementations
// =============================================================================

SEXP qmc_get_target_indicators(const SEXP R_target,
                               const SEXP R_treatments)
{
	if (!isLogical(R_target)) {
		iqmc_error("`R_target` must be logical.");
	}
	if (!isInteger(R_treatments)) {
		iqmc_error("`R_treatments` must be integer.");
	}

	const size_t num_treatments = (size_t) xlength(R_target);
	const size_t num_observations = (size_t) xlength(R_treatments);

	const int* const target = LOGICAL(R_target);
	const int* const treatments = INTEGER(R_treatments);

	SEXP R_out_indices = PROTECT(allocVector(INTSXP, (R_xlen_t) num_observations));
	int* out_indices = INTEGER(R_out_indices);

	int out_of_bounds = 0;
	for (size_t i = 0; i < num_observations; ++i) {
		out_of_bounds += (treatments[i] < 0) + (treatments[i] >= num_treatments);
	}
	if (out_of_bounds > 0) {
		iqmc_error("Treatment out of bounds.");
	}

	for (int i = 0; i < num_observations; ++i) {
		*out_indices = i + 1;
		out_indices += (target[treatments[i]] != 0);
	}

	SETLENGTH(R_out_indices, out_indices - INTEGER(R_out_indices));

	UNPROTECT(1);
	return R_out_indices;
}
