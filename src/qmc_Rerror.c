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

#include "qmc_Rerror.h"

#include <R.h>
#include <Rinternals.h>


void qmc_Rerror__(const char* const msg,
                  const char* const file,
                  const int line) {
	char error_buffer[255];
	if (snprintf(error_buffer, 255, "(%s:%d) %s", file, line, msg) < 0) {
		error("qmc_Rerror.c: Error printing error message.");
	}
	error(error_buffer);
}
