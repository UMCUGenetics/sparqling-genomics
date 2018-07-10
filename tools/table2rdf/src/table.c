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

#include "table.h"
#include "ui.h"
#include "runtime_configuration.h"

#include <stdlib.h>
#include <string.h>

table_hdr_t *
process_header (FILE* stream, const unsigned char *origin)
{
  table_hdr_t *header = calloc (1, sizeof (table_hdr_t));
  if (!header)
    {
      ui_print_general_memory_error();
      return NULL;
    }

  header->keys = calloc (64, sizeof (char *));
  header->keys_alloc_len = 64;
  if (header->keys == NULL)
    {
      ui_print_general_memory_error();
      return NULL;
    }

  char *line = NULL;
  size_t line_len = 0;
  if (getdelim (&line, &line_len, '\n', stream) != -1)
    {
      header->keys_len = 0;
      char *token = NULL;

      token = strtok (line, config.delimiter);
      header->keys[header->keys_len] = token;

      while (token != NULL)
        {
          /* Dynamically grow the number of keys, but keep it as an
           * array so that lookups remain constant time. */
          if (header->keys_len >= header->keys_alloc_len)
            {
              header->keys_alloc_len = header->keys_alloc_len + 64;
              header->keys = realloc (header->keys,
                                      header->keys_alloc_len * sizeof (char *));

              if (header->keys == NULL)
                {
                  ui_print_general_memory_error();
                  return NULL;
                }
            }

          token = strtok (NULL, config.delimiter);
          header->keys[header->keys_len] = token;
        }

      /* The array length is the index of the last element + 1, because
       * array index starts at 0.  Here we add one to the last index. */
      header->keys_len += 1;
    }
  else
    {
      ui_print_file_read_error (origin);
    }

  return header;
}

void
process_variant (table_hdr_t* hdr, FILE *stream, const unsigned char *origin)
{
}
