/*
 * Copyright © 2020  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include <libguile.h>

bool get_printable_hash (unsigned char *hash, uint32_t length, unsigned char *output);
unsigned char *get_hash_from_file (const char *filename, gnutls_digest_algorithm_t algorithm);

SCM sha256sum_from_file (SCM filename_scm);
SCM md5sum_from_file (SCM filename_scm);
void init_hashing ();

