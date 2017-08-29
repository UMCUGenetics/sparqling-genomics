/*
 * Copyright (C) 2017  Roel Janssen <roel@gnu.org>
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef VCFHEADER_H
#define VCFHEADER_H

#include "Origin.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct
{
  char *type;
  int32_t type_len;

  char *key;
  int32_t key_len;

  char *value;
  int32_t value_len;

  Origin *origin;
  char hash[65];
} VcfHeader;

char *hash_VcfHeader (VcfHeader *g, bool use_cache);
void print_VcfHeader (VcfHeader *g);
void initialize_VcfHeader (VcfHeader *g);
void reset_VcfHeader (VcfHeader *g);

#endif  /* VCFHEADER_H */
