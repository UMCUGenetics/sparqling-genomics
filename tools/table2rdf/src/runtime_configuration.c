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
  config.input_file = NULL;
  config.caller = NULL;
  config.delimiter = "\t";
  config.header_line = NULL;
  config.output_format = NULL;
  config.column_counter = 0;
  config.row_counter = 0;
  config.show_progress_info = false;

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
