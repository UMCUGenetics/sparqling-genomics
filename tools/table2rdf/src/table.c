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
#include <ctype.h>

void
sanitize_string (char *string, uint32_t length)
{
  uint32_t index = 0;
  for (; index < length; index++)
    if (! isalnum(string[index]))
      string[index] = '_';
}

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
  header->column_ids = calloc (64, sizeof (char *));
  header->keys_alloc_len = 64;
  if (header->keys == NULL || header->column_ids == NULL)
    {
      ui_print_general_memory_error();
      return NULL;
    }

  char *line = NULL;
  size_t line_len = 0;
  if (getdelim (&line, &line_len, '\n', stream) != -1)
    {
      /* The 'getdelim' function does not remove the delimiter, so let's do
       * that here. */
      size_t line_strlen = strlen (line);
      if (line[line_strlen - 1] == '\n')
        line[line_strlen - 1] = '\0';

      header->keys_len = 0;
      char *token = NULL;

      token = strtok (line, config.delimiter);
      header->keys[header->keys_len] = token;

      raptor_statement *stmt;
      while (token != NULL)
        {
          /* Dynamically grow the number of keys, but keep it as an
           * array so that lookups remain constant time. */
          if (header->keys_len >= header->keys_alloc_len)
            {
              header->keys_alloc_len = header->keys_alloc_len + 64;
              header->keys = realloc (header->keys,
                                      header->keys_alloc_len * sizeof (char *));
              header->column_ids = realloc (header->column_ids,
                                            header->keys_alloc_len * sizeof (char *));

              if (header->keys == NULL || header->column_ids == NULL)
                {
                  ui_print_general_memory_error();
                  return NULL;
                }
            }

          header->keys[header->keys_len] = token;
          if (! generate_column_id (origin, config.id_buf))
            {
              ui_print_general_memory_error();
              return NULL;
            }

          header->column_ids[header->keys_len] = strdup (config.id_buf);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, config.id_buf);
          stmt->predicate = term (PREFIX_RDF, "#type");
          stmt->object    = class (CLASS_COLUMN);
          register_statement (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, config.id_buf);
          stmt->predicate = term (PREFIX_BASE, "originatedFrom");
          stmt->object    = term (PREFIX_BASE, (char *)origin);
          register_statement (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, config.id_buf);
          stmt->predicate = term (PREFIX_RDFS, "#label");
          stmt->object    = literal (token, XSD_STRING);
          register_statement (stmt);

          snprintf (config.number_buffer, 32, "%u", header->keys_len);
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, config.id_buf);
          stmt->predicate = term (PREFIX_BASE, "position");
          stmt->object    = literal (config.number_buffer, XSD_INTEGER);
          register_statement (stmt);

          sanitize_string (header->keys[header->keys_len],
                           strlen (header->keys[header->keys_len]));

          header->keys_len += 1;
          token = strtok (NULL, config.delimiter);
        }

      /* The array length is the index of the last element + 1, because
       * array index starts at 0.  Here we add one to the last index. */
      header->keys_len += 1;
    }
  else
    {
      ui_print_file_read_error ((char *)origin);
    }

  free (line);
  return header;
}

void
process_row (table_hdr_t* hdr, FILE *stream, const unsigned char *origin)
{
  char *line      = NULL;
  size_t line_len = 0;

  if (getdelim (&line, &line_len, '\n', stream) != -1)
    {
      /* The 'getdelim' function does not remove the delimiter, so let's do
       * that here. */
      size_t line_strlen = strlen (line);
      if (line[line_strlen - 1] == '\n')
        line[line_strlen - 1] = '\0';

      char *token            = NULL;
      raptor_statement *stmt = NULL;

      if (! generate_row_id (origin, config.id_buf))
        {
          ui_print_general_memory_error();
          return;
        }

      token = strtok (line, config.delimiter);
      uint32_t column_index = 0;
      for (; column_index < hdr->keys_len; column_index++)
        {
          if (token != NULL)
            {
              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = term (PREFIX_COLUMN, config.id_buf);
              stmt->predicate = term (PREFIX_RDF, "#type");
              stmt->object    = class (CLASS_ROW);
              register_statement (stmt);

              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = term (PREFIX_COLUMN, config.id_buf);
              stmt->predicate = term (PREFIX_BASE, "originatedFrom");
              stmt->object    = term (PREFIX_BASE, (char *)origin);
              register_statement (stmt);

              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = term (PREFIX_COLUMN, config.id_buf);
              stmt->predicate = term (PREFIX_COLUMN, hdr->column_ids[column_index]);
              /* TODO: Detect integers as integers, floats as floats,
               * booleans as booleans, and the rest as strings. */
              stmt->object    = literal (token, XSD_STRING);
              register_statement (stmt);
            }

          token = strtok (NULL, config.delimiter);
        }
    }
  else
    {
      ui_print_file_read_error ((char *)origin);
    }

  free (line);
}
