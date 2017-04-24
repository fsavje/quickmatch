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

#ifndef QMC_ERROR_HG
#define QMC_ERROR_HG

#define iqmc_error(msg) (iqmc_error__(msg, __FILE__, __LINE__))

#define iqmc_assert(expression) (void)((expression) || (iqmc_error__("Failed assert: `" #expression "`.", __FILE__, __LINE__), 0))

void iqmc_error__(const char* msg,
                  const char* file,
                  int line);

#endif // ifndef QMC_ERROR_HG
