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

#ifndef SAMPLE_H
#define SAMPLE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct
{
  char *filename;
  int32_t filename_len;
  char hash[65];
} Sample;

char *hash_Sample (Sample *g, bool use_cache);
void print_Sample (Sample *g);
void initialize_Sample (Sample *g);
void reset_Sample (Sample *g);
bool set_Sample_filename (Sample *s, const char *filename);
#endif  /* SAMPLE_H */
