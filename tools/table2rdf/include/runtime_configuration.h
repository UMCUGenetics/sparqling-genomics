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

#ifndef RUNTIMECONFIGURATION_H
#define RUNTIMECONFIGURATION_H

/*
 * This object provides the basic infrastructure to make the rest of the
 * program more efficient or more convenient to write.
 */

#include "ontology.h"

#include <stdbool.h>
#include <stdint.h>
#include <raptor2.h>

/* The program uses a base ontology for everything that cannot be
 * expressed using an existing ontology.  We store the base ontology URI
 * in this constant, so that it can be used as a macro in the remainder
 * of the code. */
#define ONTOLOGY_URI "http://rdf.umcutrecht.nl/table2rdf/"

/* In this program, a couple of URI prefixes are stored in
 * 'program_config.uris'.  To get an idea of which URI is which, the
 * following named index can be used.  So for example,
 * program_config.uris[URI_FALDO_PREFIX] contains the URI prefix for FALDO. */
#define URI_ONTOLOGY_PREFIX           0
#define URI_RDF_PREFIX                1
#define URI_RDFS_PREFIX               2
#define URI_OWL_PREFIX                3
#define URI_XSD_PREFIX                4
#define URI_FALDO_PREFIX              5
#define URI_SAMPLE_PREFIX             6

/* The following integer is used to determine the size of the constants.
 * Please adjust accordingly when you change the first or the last
 * constant.
 */
#define NUMBER_OF_URIS           (URI_SAMPLE_PREFIX + 1)

/* In addition to URIs, we also have NODEs that are commonly used throughout
 * the remainder of the program.  We use a similar approach as the URIs to
 * accomodate common nodes. */
#define NODE_RDF_TYPE                 0
#define NODE_ORIGIN_CLASS             1
#define NODE_SAMPLE_CLASS             2

#define NUMBER_OF_NODES          (NODE_SAMPLE_CLASS + 1)

/* In addition to URIs and nodes, “datatype property nodes” can contain literal
 * values.  These have a type, which is often described in the xsd namespace.
 *
 * HTSlib has constants for valid datatypes in the variant call format (VCF).
 * Keeping them in sync makes life easier.
 */
#define TYPE_STRING              0
#define TYPE_INTEGER             1
#define TYPE_FLOAT               2
#define TYPE_BOOLEAN             3

#define NUMBER_OF_TYPES          4

/* This struct can be used to make program options available throughout the
 * entire code without needing to pass them around as parameters.  Do not write
 * to these values, other than in the runtime_configuration_init() and
 * ui_process_command_line() functions. */
typedef struct
{
  /* Command-line configurable options. */
  char              *input_file;
  char              *caller;
  char              *output_format;
  char              *delimiter;
  char              *secondary_delimiter;
  char              *header_line;
  char              *sample_name;
  char              **transformers_buffer;
  char              **transformer_keys;
  char              **transformer_values;
  uint32_t          transformers_buffer_len;
  uint32_t          transformers_buffer_alloc_len;
  uint32_t          transformer_alloc_len;
  uint32_t          transformer_len;
  bool              show_progress_info;
  bool              skip_first_line;
  bool              input_from_stdin;

  /* Raptor-specifics */
  raptor_world      *raptor_world;
  raptor_serializer *raptor_serializer;
  raptor_uri        **prefix;

  /* Application-specific ontology. */
  ontology_t        *ontology;

  /* Shared buffers. */
  uint32_t          column_counter;
  uint32_t          row_counter;
  uint32_t          prefix_name_counter;
  char              id_buf[77];
  char              number_buffer[32];
} RuntimeConfiguration;


/* This is where we can set default values for the program's options. */
RuntimeConfiguration config;

bool runtime_configuration_init (void);
bool runtime_configuration_redland_init (void);
void runtime_configuration_free (void);
void runtime_configuration_redland_free (void);

bool generate_column_id (const unsigned char *origin, char *column_id);
bool generate_row_id (const unsigned char *origin, char *row_id);
bool generate_prefix_name (unsigned char *prefix_name);
bool preregister_transformer (const char *pair);
bool register_transformers ();

#endif  /* RUNTIMECONFIGURATION_H */
