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

#ifndef GENOMEPOSITION_H
#define GENOMEPOSITION_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef enum
{
  DIRECTION_UNKNOWN,
  DIRECTION_3_TO_5,
  DIRECTION_5_TO_3
} DirectionType;

typedef struct
{
  char *chromosome;
  uint32_t chromosome_len;
  uint32_t position;
  char hash[65];

  int32_t cipos_len;
  int32_t *cipos;

  char *reference;
  DirectionType direction;
} GenomePosition;

char *hash_GenomePosition (GenomePosition *g, bool use_cache);
void print_GenomePosition (GenomePosition *g);
void initialize_GenomePosition (GenomePosition *g);
void reset_GenomePosition (GenomePosition *g);

#endif  /* GENOMEPOSITION_H */
