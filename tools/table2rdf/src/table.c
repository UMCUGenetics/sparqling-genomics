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

/* This function replaces non-alphanumeric characters with
 * underscores, and all uppercase characters with their lowercase
 * equivalent. */
char *
sanitize_string (const char *string, uint32_t length)
{
  if (string == NULL || length < 1)
    return NULL;

  char *output = strdup (string);
  if (output == NULL)
    return NULL;

  uint32_t index = 0;
  for (; index < length; index++)
    {
      if (! isalnum(output[index]))
        output[index] = '_';
      else
        output[index] = tolower (output[index]);
    }

  return output;
}


/* This function removes single quotes and double quotes
 * in the first, and last position of a string. */
char *
trim_quotes (const char *string, uint32_t length)
{
  if (string == NULL || length < 1)
    return NULL;

  char *output = strdup (string);
  if (output == NULL)
    return NULL;

  if (output[length - 1] == '"' || output[length - 1] == '\'')
    output[length - 1] = '\0';

  if (output[0] == '"' || output[0] == '\'')
    {
      uint32_t index = 1;
      for (; index < length; index++)
        output[index - 1] = output[index];

      output[length - 1] = '\0';
    }

  return output;
}

table_hdr_t *
process_header (FILE* stream, const unsigned char *origin, const char *filename)
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
  ssize_t result = 0;

  if (config.header_line == NULL)
    result = getdelim (&line, &line_len, '\n', stream);
  else
    line = config.header_line;

  if (!(result == -1 && !config.header_line))
    {
      /* The 'getdelim' function does not remove the delimiter, so let's do
       * that here. */
      size_t line_strlen = strlen (line);
      if (line[line_strlen - 1] == '\n')
        line[line_strlen - 1] = '\0';

      header->keys_len = 0;
      char *token = NULL;

      if (config.header_line)
        token = strtok (line, ";");
      else
        token = strtok (line, config.delimiter);

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

          header->keys[header->keys_len] = trim_quotes (token, strlen (token));
          char *column_id = sanitize_string (header->keys[header->keys_len],
                                             strlen (header->keys[header->keys_len]));

          if (! column_id)
            {
              ui_print_general_memory_error();
              return NULL;
            }

          header->column_ids[header->keys_len] = column_id;

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, column_id);
          stmt->predicate = term (PREFIX_RDF, "#type");
          stmt->object    = class (CLASS_COLUMN);
          register_statement (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, column_id);
          stmt->predicate = term (PREFIX_BASE, "foundIn");
          stmt->object    = term (PREFIX_BASE, (char *)origin);
          register_statement (stmt);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, column_id);
          stmt->predicate = term (PREFIX_RDFS, "#label");
          stmt->object    = literal (header->keys[header->keys_len],
                                     XSD_STRING);
          register_statement (stmt);

          snprintf (config.number_buffer, 32, "%u", header->keys_len);
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_COLUMN, column_id);
          stmt->predicate = term (PREFIX_BASE, "position");
          stmt->object    = literal (config.number_buffer, XSD_INTEGER);
          register_statement (stmt);

          header->keys_len += 1;

          if (config.header_line)
            token = strtok (NULL, ";");
          else
            token = strtok (NULL, config.delimiter);
        }

      /* The array length is the index of the last element + 1, because
       * array index starts at 0.  Here we add one to the last index. */
      header->keys_len += 1;
    }
  else
    {
      ui_print_file_read_error ((char *)filename);
    }

  if (config.header_line == NULL)
    free (line);

  return header;
}

void
process_row (table_hdr_t* hdr, FILE *stream, const unsigned char *origin, const char *filename)
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
      char *trimmed_token    = NULL;
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
              trimmed_token = trim_quotes (token, strlen (token));

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
              stmt->object    = literal (trimmed_token, XSD_STRING);
              register_statement (stmt);

              free (trimmed_token);
              trimmed_token = NULL;
            }

          token = strtok (NULL, config.delimiter);
        }
    }
  else if (!feof (stream))
    ui_print_file_read_error ((char *)filename);

  free (line);
}
