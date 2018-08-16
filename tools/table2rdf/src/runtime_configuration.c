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

#include "runtime_configuration.h"
#include "ui.h"
#include "helper.h"
#include "ontology.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool
runtime_configuration_init (void)
{
  config.input_file = NULL;
  config.caller = NULL;
  config.delimiter = "\t";
  config.secondary_delimiter = NULL;
  config.header_line = NULL;
  config.sample_name = NULL;
  config.output_format = NULL;
  config.transformers_buffer = NULL;
  config.transformer_keys = NULL;
  config.transformer_values = NULL;
  config.transformers_buffer_len = 0;
  config.transformer_len = 0;
  config.transformers_buffer_alloc_len = 0;
  config.transformer_alloc_len = 0;
  config.column_counter = 0;
  config.row_counter = 0;
  config.prefix_name_counter = 0;
  config.show_progress_info = false;
  config.skip_first_line = false;
  config.input_from_stdin = false;

  return true;
}

bool
runtime_configuration_redland_init (void)
{
  if (!config.output_format)
    config.output_format = "turtle";

  config.raptor_world      = raptor_new_world();
  config.raptor_serializer = raptor_new_serializer (config.raptor_world,
                                                    config.output_format);

  if (!config.raptor_world || !config.raptor_serializer)
    return (ui_print_redland_error () == 0);

  raptor_serializer_start_to_file_handle (config.raptor_serializer, NULL, stdout);
  if (!ontology_init (&(config.ontology)))
    return (ui_print_redland_error () == 0);

  if (!register_transformers ())
    return (ui_print_redland_error () == 0);

  return true;
}

void
runtime_configuration_redland_free (void)
{
  /* Free the Redland-allocated memory. */
  ontology_free (config.ontology);

  uint32_t index = 0;
  for (; index < config.transformer_len; index++)
    {
      /* We do not have to clean up transformer_values,
       * because it was allocated in one go. */
      free (config.transformer_keys[index]);
    }

  free (config.transformer_keys);
  free (config.transformer_values);

  for (index = 0; index < config.transformers_buffer_len; index++)
    free (config.transformers_buffer[index]);

  free (config.transformers_buffer);
}

void
runtime_configuration_free (void)
{
  runtime_configuration_redland_free ();

  raptor_serializer_serialize_end (config.raptor_serializer);
  raptor_free_serializer (config.raptor_serializer);
  raptor_free_world (config.raptor_world);
}

bool
generate_column_id (const unsigned char *origin, char *column_id)
{
  int32_t bytes_written;
  bytes_written = snprintf (column_id, 77, "%s-C%010u",
                            origin,
                            config.column_counter);

  config.column_counter++;
  return (bytes_written > 0);
}

bool
generate_row_id (const unsigned char *origin, char *row_id)
{
  int32_t bytes_written;
  bytes_written = snprintf (row_id, 77, "%s-R%010u",
                            origin,
                            config.row_counter);

  config.row_counter++;
  return (bytes_written > 0);
}

bool
generate_prefix_name (unsigned char *prefix_name)
{
  int32_t bytes_written;
  bytes_written = snprintf ((char *)prefix_name, 12, "p%010u",
                            config.prefix_name_counter);

  config.prefix_name_counter++;
  return (bytes_written > 0);
}

bool
preregister_transformer (const char *pair)
{
  /* Always make sure there are enough indexes. */
  if (config.transformers_buffer_alloc_len == 0 ||
      config.transformers_buffer_len == config.transformers_buffer_alloc_len - 1)
    {
      char **buffer = realloc (config.transformers_buffer,
                               config.transformers_buffer_alloc_len +
                               sizeof (char *) * 32);
      if (buffer == NULL) return false;
      config.transformers_buffer = buffer;
    }

  char *duplicate = strdup (pair);
  if (duplicate == NULL)
    return false;

  config.transformers_buffer[config.transformers_buffer_len] = duplicate;
  config.transformers_buffer_len += 1;

  return true;
}

bool
register_transformers ()
{
  uint32_t index = 0;
  for (; index < config.transformers_buffer_len; index++)
    {
      char *trans = config.transformers_buffer[index];
      char *separator = strchr (trans, '=');
      if (separator == NULL)
        {
          fprintf (stderr, "Warning: Ignoring invalid transformer separator: '%s'\n", trans);
          continue;
        }

      char *value = separator + sizeof (char);
      *separator = '\0';

      /* Always make sure there are enough indexes. */
      if (config.transformer_alloc_len == 0 ||
          config.transformer_len == config.transformer_alloc_len - 1)
        {
          char **keys = realloc (config.transformer_keys,
                                 config.transformer_alloc_len +
                                 sizeof (char *) * 32);
          if (keys == NULL) return false;
          config.transformer_keys = keys;

          char **values = realloc (config.transformer_values,
                                   config.transformer_alloc_len +
                                   sizeof (char *) * 32);

          if (values == NULL) return false;
          config.transformer_values = values;

          if (config.transformer_keys == NULL ||
              config.transformer_values == NULL)
            return false;
        }

      config.ontology->prefixes_length += 1;
      raptor_uri **temp = realloc (config.ontology->prefixes,
                                   config.ontology->prefixes_length * sizeof (raptor_uri*));
      if (temp == NULL)
        {
          config.ontology->prefixes_length -= 1;
          return false;
        }

      config.ontology->prefixes = temp;

      unsigned char prefix_name[12];
      if (!generate_prefix_name (prefix_name))
        return false;

      config.transformer_keys[config.transformer_len] =
        sanitize_string (trans, ((separator - trans) * sizeof (char)));

      config.transformer_values[config.transformer_len] = value;
      config.ontology->prefixes[config.ontology->prefixes_length - 1] =
        raptor_new_uri (config.raptor_world, (unsigned char *)value);

      raptor_serializer_set_namespace (config.raptor_serializer,
                                       config.ontology->prefixes[config.ontology->prefixes_length - 1],
                                       prefix_name);

      config.transformer_len += 1;
    }

  return true;
}
