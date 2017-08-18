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

#ifndef RUNTIMECONFIGURATION_H
#define RUNTIMECONFIGURATION_H

#include <stdbool.h>

/* Note: The HASH_LENGTH will be 32, but a pretty-printed hexadecimal version
 * of it will need 64 bytes of space. */
#define HASH_ALGORITHM GCRY_MD_SHA256
#define HASH_LENGTH    gcry_md_get_algo_dlen (HASH_ALGORITHM)

/* This struct can be used to make program options available throughout the
 * entire code without needing to pass them around as parameters.  Do not write
 * to these values, other than in the option parser inside main(). */
typedef struct
{
  char *filter;
  char *keep;
  char *input_file;
  char *graph_location;
} RuntimeConfiguration;


/* This is where we can set default values for the program's options. */
RuntimeConfiguration program_config;

#endif  /* RUNTIMECONFIGURATION_H */
