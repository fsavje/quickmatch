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

#include "qmc_potential_outcomes.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include "qmc_Rerror.h"


// =============================================================================
// External function implementations
// =============================================================================

SEXP qmc_potential_outcomes(const SEXP R_outcomes,
                            const SEXP R_treatments,
                            const SEXP R_matching,
                            const SEXP R_targets,
                            const SEXP R_subset)
{
	if (!isReal(R_outcomes)) {
		qmc_Rerror("`R_outcomes` must be numeric.");
	}
	if (!isInteger(R_treatments)) {
		qmc_Rerror("`R_treatments` must be integer.");
	}
	if (xlength(R_treatments) != xlength(R_outcomes)) {
		qmc_Rerror("`R_treatments` and `R_outcomes` must be same length.");
	}
	if (!isInteger(R_matching)) {
		qmc_Rerror("`R_matching` must be integer.");
	}
	if (xlength(R_matching) != xlength(R_outcomes)) {
		qmc_Rerror("`R_matching` and `R_outcomes` must be same length.");
	}
	if (!isInteger(getAttrib(R_matching, install("cluster_count")))) {
		qmc_Rerror("`R_matching` is not valid `Rscc_clustering` object.");
	}
	if (asInteger(getAttrib(R_matching, install("cluster_count"))) <= 0) {
		qmc_Rerror("`R_matching` is empty.");
	}
	if (!isLogical(R_targets)) {
		qmc_Rerror("`R_targets` must be logical.");
	}
	if (!isNull(R_subset) && !isInteger(R_subset)) {
		qmc_Rerror("`R_subset` must be NULL or integer.");
	}

	const size_t num_observations = (size_t) xlength(R_outcomes);
	const size_t num_groups = (size_t) asInteger(getAttrib(R_matching, install("cluster_count")));
	const size_t num_treatments = (size_t) xlength(R_targets);

	const double* const outcomes = REAL(R_outcomes);
	const int* const matching = INTEGER(R_matching);
	const int* const treatments = INTEGER(R_treatments);
	const int* const targets = LOGICAL(R_targets);
	const size_t len_subset = (size_t) xlength(R_subset);
	const int* const subset = isNull(R_subset) ? NULL : INTEGER(R_subset);

	SEXP R_out_means = PROTECT(allocVector(REALSXP, (R_xlen_t) num_treatments));
	double* const out_means = REAL(R_out_means);
	uint32_t* const weight_count = calloc(num_groups, sizeof(uint32_t));
	uint32_t* const treatment_count = calloc(num_groups * num_treatments, sizeof(uint32_t));
	double* const treatment_outcome_sum = calloc(num_groups * num_treatments, sizeof(double));
	if (weight_count == NULL ||
			treatment_count == NULL ||
			treatment_outcome_sum == NULL) {
		free(weight_count);
		free(treatment_count);
		free(treatment_outcome_sum);
		qmc_Rerror("Out of memory.");
	}

	for (size_t i = 0; i < num_observations; ++i) {
		if (matching[i] == NA_INTEGER) {
			continue;
		}
		if (matching[i] < 0 || matching[i] >= num_groups) {
			qmc_Rerror("Matching out of bounds.");
		}
		if (treatments[i] < 0 || treatments[i] >= num_treatments) {
			qmc_Rerror("Treatment out of bounds.");
		}
		const size_t tmp_index = ((size_t) treatments[i]) * num_groups + matching[i];
		++treatment_count[tmp_index];
		treatment_outcome_sum[tmp_index] += outcomes[i];
	}

	if (subset == NULL) {
		for (size_t i = 0; i < num_observations; ++i) {
			if (matching[i] != NA_INTEGER) {
				++weight_count[matching[i]];
			} else {
				qmc_Rerror("Cannot include unmatched units in estimation.");
			}
		}
	} else {
		for (size_t i = 0; i < len_subset; ++i) {
			if (matching[subset[i]] != NA_INTEGER) {
				++weight_count[matching[subset[i]]];
			} else {
				qmc_Rerror("Cannot include unmatched units in estimation.");
			}
		}
	}

	uint64_t total_weight_count = 0;
	for (size_t g = 0; g < num_groups; ++g) {
		total_weight_count += weight_count[g];
	}

	for (size_t t = 0; t < num_treatments; ++t) {
		if (!targets[t]) {
			out_means[t] = NA_REAL;
		} else {
			out_means[t] = 0.0;
			const size_t t_add = t * num_groups;
			for (size_t g = 0; g < num_groups; ++g) {
				if (weight_count[g] > 0) {
					if (treatment_count[t_add + g] == 0) {
						out_means[t] = NA_REAL;
						break;
					} else {
						out_means[t] += ((double) weight_count[g]) *
						                   treatment_outcome_sum[t_add + g] /
						                   ((double) treatment_count[t_add + g]);
					}
				}
			}
			if (!ISNA(out_means[t])) {
				 out_means[t] /= ((double) total_weight_count);
			}
		}
	}

	free(weight_count);
	free(treatment_count);
	free(treatment_outcome_sum);

	UNPROTECT(1);
	return R_out_means;
}
