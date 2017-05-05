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

#include "matching_weights.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include "error.h"


// =============================================================================
// External function implementations
// =============================================================================

SEXP qmc_matching_weights(const SEXP R_treatments,
                          const SEXP R_num_treatments,
                          const SEXP R_matching,
                          const SEXP R_subset)
{
	if (!isInteger(R_treatments)) {
		iqmc_error("`R_treatments` must be integer.");
	}
	if (!isInteger(R_num_treatments)) {
		iqmc_error("`R_num_treatments` must be integer.");
	}
	if (asInteger(R_num_treatments) < 2) {
		iqmc_error("Must be at least two treatment conditions.");
	}
	if (!isInteger(R_matching)) {
		iqmc_error("`R_matching` must be integer.");
	}
	if (!isInteger(getAttrib(R_matching, install("cluster_count")))) {
		iqmc_error("`R_matching` is not valid `scclust` object.");
	}
	if (asInteger(getAttrib(R_matching, install("cluster_count"))) <= 0) {
		iqmc_error("`R_matching` is empty.");
	}
	if (xlength(R_matching) != xlength(R_treatments)) {
		iqmc_error("`R_matching` and `R_treatments` must be same length.");
	}
	if (!isNull(R_subset) && !isInteger(R_subset) && !isLogical(R_subset)) {
		iqmc_error("`R_subset` must be NULL, integer or logical.");
	}
	if (isLogical(R_subset) && (xlength(R_subset) != xlength(R_treatments))) {
		iqmc_error("`R_subset` and `R_treatments` must be same length when `R_subset` is logical.");
	}

	// R objects to C
	const size_t num_observations = (size_t) xlength(R_treatments);
	const uint32_t num_treatments = (uint32_t) asInteger(R_num_treatments);
	// unmatched units in group 0, the rest `group ID + 1`
	const size_t num_groups = (size_t) asInteger(getAttrib(R_matching, install("cluster_count"))) + 1;
	const int* const treatments = INTEGER(R_treatments);
	const int* const matching = INTEGER(R_matching);

	// Bounds and sanity checks
	{
		uint32_t treatments_bc = 0;
		uint32_t matching_bc = 0;
		for (size_t i = 0; i < num_observations; ++i) {
			treatments_bc += (treatments[i] <= 0) + (treatments[i] > num_treatments);
			matching_bc += (matching[i] != NA_INTEGER) * ((matching[i] < 0) + ((matching[i] + 1) >= num_groups));
		}
		if (treatments_bc != 0) {
			iqmc_error("Treatment out of bounds.");
		}
		if (matching_bc != 0) {
			iqmc_error("Matching out of bounds.");
		}
		if (isInteger(R_subset)) {
			const size_t len_subset = (size_t) xlength(R_subset);
			const int* const subset = INTEGER(R_subset);
			uint32_t subset_bc = 0;
			for (size_t i = 0; i < len_subset; ++i) {
				subset_bc += (subset[i] <= 0) + (subset[i] > num_observations);
			}
			if (subset_bc != 0) {
				iqmc_error("Subset out of bounds.");
			}
		}
	}

	// Allocate output variables
	SEXP R_unit_weights = PROTECT(allocVector(REALSXP, (R_xlen_t) num_observations));
	SEXP R_treatment_missing = PROTECT(allocVector(LGLSXP, (R_xlen_t) num_treatments));

	double* const unit_weights = REAL(R_unit_weights);
	int* const treatment_missing = LOGICAL(R_treatment_missing);

	// Allocate working memory
	uint32_t* const subset_count = calloc(num_groups, sizeof(uint32_t));
	uint32_t* const treatment_count = calloc(num_groups * num_treatments, sizeof(uint32_t));
	double* const block_treatment_weight = malloc(sizeof(double[num_groups * num_treatments]));

	if (subset_count == NULL || treatment_count == NULL || block_treatment_weight == NULL) {
		free(subset_count);
		free(treatment_count);
		free(block_treatment_weight);
		iqmc_error("Out of memory.");
	}

	if (isNull(R_subset)) {
		for (size_t i = 0; i < num_observations; ++i) {
			++subset_count[(matching[i] != NA_INTEGER) * (matching[i] + 1)];
		}
	} else if (isInteger(R_subset)) {
		const size_t len_subset = (size_t) xlength(R_subset);
		const int* const subset = INTEGER(R_subset);
		for (size_t i = 0; i < len_subset; ++i) {
			++subset_count[(matching[subset[i] - 1] != NA_INTEGER) * (matching[subset[i] - 1] + 1)];
		}
	} else { // isLogical(R_subset)
		iqmc_assert(((size_t) xlength(R_subset)) == num_observations);
		const int* const subset = LOGICAL(R_subset);
		for (size_t i = 0; i < num_observations; ++i) {
			subset_count[(matching[i] != NA_INTEGER) * (matching[i] + 1)] += (subset[i] != 0);
		}
	}
	if (subset_count[0] > 0) {
		warning("Some units in subset are unmatched. They will be ignored.");
	}

	uint64_t total_subset_count = 0;
	for (size_t g = 1; g < num_groups; ++g) {
		total_subset_count += subset_count[g];
	}

	// Count number of units assigned to each treatment in each matched group
	for (size_t i = 0; i < num_observations; ++i) {
		++treatment_count[((treatments[i] - 1) * num_groups) + ((matching[i] != NA_INTEGER) * (matching[i] + 1))];
	}

	for (uint32_t t = 0; t < num_treatments; ++t) {
		const size_t t_add = t * num_groups;
		treatment_missing[t] = 0;
		block_treatment_weight[t_add] = 0.0; // Unassigned units
		for (size_t g = 1; g < num_groups; ++g) {
			if (subset_count[g] == 0) {
				block_treatment_weight[t_add + g] = 0.0;
			} else if (treatment_count[t_add + g] == 0) { // subset_count[g] > 0
				treatment_missing[t] = 1;
				block_treatment_weight[t_add + g] = NA_REAL;
			} else { // subset_count[g] > 0 && treatment_count[t_add + g] > 0
				block_treatment_weight[t_add + g] = ((double) subset_count[g]) / ((double) treatment_count[t_add + g]);
			}
		}
	}

	for (size_t i = 0; i < num_observations; ++i) {
		unit_weights[i] = block_treatment_weight[((treatments[i] - 1) * num_groups) + ((matching[i] != NA_INTEGER) * (matching[i] + 1))];
	}

	// Free working memory and return
	free(subset_count);
	free(treatment_count);
	free(block_treatment_weight);

	SEXP R_weight_obj = PROTECT(allocVector(VECSXP, 3));
	SET_VECTOR_ELT(R_weight_obj, 0, R_unit_weights);
	SET_VECTOR_ELT(R_weight_obj, 1, ScalarReal((double) total_subset_count));
	SET_VECTOR_ELT(R_weight_obj, 2, R_treatment_missing);

	SEXP R_weight_obj_names = PROTECT(allocVector(STRSXP, 3));
	SET_STRING_ELT(R_weight_obj_names, 0, mkChar("unit_weights"));
	SET_STRING_ELT(R_weight_obj_names, 1, mkChar("total_subset_count"));
	SET_STRING_ELT(R_weight_obj_names, 2, mkChar("treatment_missing"));
	setAttrib(R_weight_obj, R_NamesSymbol, R_weight_obj_names);

	UNPROTECT(4);
	return R_weight_obj;
}
