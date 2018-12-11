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

bool
runtime_configuration_init (void)
{
  config.filter = NULL;
  config.keep = NULL;
  config.input_file = NULL;
  config.reference = NULL;
  config.caller = NULL;
  config.output_format = NULL;
  config.user_hash = NULL;
  config.non_unique_variant_counter = 0;
  config.info_field_indexes = NULL;
  config.info_field_indexes_len = 0;
  config.info_field_indexes_blocks = 0;
  config.format_field_indexes = NULL;
  config.format_field_indexes_len = 0;
  config.format_field_indexes_blocks = 0;
  config.header_only = false;
  config.metadata_only = false;
  config.show_progress_info = false;
  config.process_info_fields = true;
  config.process_format_fields = true;
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

  return true;
}

void
runtime_configuration_redland_free (void)
{
  /* Free the Redland-allocated memory. */
  ontology_free (config.ontology);
}

void
runtime_configuration_free (void)
{
  runtime_configuration_redland_free ();

  /* Free caches. */
  if (config.info_field_indexes != NULL)
    {
      free (config.info_field_indexes);
      config.info_field_indexes = NULL;
    }

  if (config.format_field_indexes != NULL)
    {
      free (config.format_field_indexes);
      config.format_field_indexes = NULL;
    }

  raptor_serializer_serialize_end (config.raptor_serializer);
  raptor_free_serializer (config.raptor_serializer);
  raptor_free_world (config.raptor_world);
}

bool
generate_variant_id (const unsigned char *origin, char *variant_id)
{
  int32_t bytes_written;
  bytes_written = snprintf (variant_id, 77, "%s-V%010u",
                            origin,
                            config.non_unique_variant_counter);

  config.non_unique_variant_counter++;
  return (bytes_written > 0);
}
