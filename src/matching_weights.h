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

#ifndef QMC_MATCHING_WEIGHTS_HG
#define QMC_MATCHING_WEIGHTS_HG

#include <R.h>
#include <Rinternals.h>

SEXP qmc_matching_weights(SEXP R_treatments,
                          SEXP R_num_treatments,
                          SEXP R_matching,
                          SEXP R_target);

#endif // ifndef QMC_MATCHING_WEIGHTS_HG
