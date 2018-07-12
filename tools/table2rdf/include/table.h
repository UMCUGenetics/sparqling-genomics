/*
 * Copyright (C) 2018  Roel Janssen <roel@gnu.org>
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

#ifndef TABLE_H
#define TABLE_H

#include <stdio.h>
#include <stdint.h>

typedef struct {
  char *header_line;
  char **column_ids;
  char **keys;
  uint32_t keys_len;
  uint32_t keys_alloc_len;
} table_hdr_t;

table_hdr_t *process_header (FILE* stream, const unsigned char *origin, const char *filename);
void process_row (table_hdr_t* hdr, FILE *stream, const unsigned char *origin, const char *filename);

char *trim_quotes (const char *string, uint32_t length);
char *sanitize_string (const char *string, uint32_t length);

#endif /* TABLE_H */
