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

#ifndef HELPER_H
#define HELPER_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <htslib/vcf.h>

#include "GenomePosition.h"

bool get_pretty_hash (unsigned char *hash, uint32_t length, char *output);


typedef struct
{
  bool is_reversed;
  bool is_left_of_ref;
  char *chromosome;
  int32_t chromosome_len;
  int32_t position;
} BndProperties;

void bnd_properties_init (BndProperties *properties);

bool parse_properties (BndProperties *properties,
                       const char *ref, int32_t ref_len,
                       const char *alt, int32_t alt_len);

bool determine_confidence_interval (FaldoExactPosition *base,
                                    const char *property,
                                    FaldoExactPosition *begin,
                                    FaldoExactPosition *end,
                                    bcf_hdr_t *header,
                                    bcf1_t *buffer);

#endif  /* HELPER_H */
