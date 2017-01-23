/* =============================================================================
 * quickmatch -- Fast Matching in Large Data Sets
 * https://github.com/fsavje/quickmatch
 *
 * Copyright (C) 2016  Fredrik Savje -- http://fredriksavje.com
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

#include "qmc_utilities.h"

#include <stddef.h>
#include <R.h>
#include <Rinternals.h>
#include "qmc_Rerror.h"


// =============================================================================
// External function implementations
// =============================================================================

SEXP qmc_translate_targets(const SEXP R_targets,
                           const SEXP R_treatments)
{
	if (!isLogical(R_targets)) {
		qmc_Rerror("`R_targets` must be logical.");
	}
	if (!isInteger(R_treatments)) {
		qmc_Rerror("`R_treatments` must be integer.");
	}

	const size_t num_treatments = (size_t) xlength(R_targets);
	const size_t num_observations = (size_t) xlength(R_treatments);

	const int* const targets = LOGICAL(R_targets);
	const int* const treatments = INTEGER(R_treatments);

	SEXP R_out_indices = PROTECT(allocVector(INTSXP, (R_xlen_t) num_observations));
	int* out_indices = INTEGER(R_out_indices);

	for (int i = 0; i < num_observations; ++i) {
		if (treatments[i] < 0 || treatments[i] >= num_treatments) {
			qmc_Rerror("Treatment out of bounds.");
		}
		*out_indices = i + 1;
		out_indices += targets[treatments[i]];
	}

	SETLENGTH(R_out_indices, out_indices - INTEGER(R_out_indices));

	UNPROTECT(1);
	return R_out_indices;
}
