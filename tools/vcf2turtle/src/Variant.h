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

#ifndef VARIANT_H
#define VARIANT_H

#include "GenomePosition.h"
#include <stdint.h>
#include <stdbool.h>

typedef struct
{
  GenomePosition *position1;
  GenomePosition *position2;
  float quality;
  char *filter;
  char *type;
  uint32_t type_len;
  char *hash;
} Variant;

char *hash_Variant (Variant *v, bool use_cache);
void print_Variant (Variant *v);

#endif  /* VARIANT_H */
