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

#include <stdbool.h>
#include <stdint.h>
#include <librdf.h>

/* In this program, a couple of URI prefixes are stored in
 * 'program_config.uris'.  To get an idea of which URI is which, the
 * following named index can be used.  So for example,
 * program_config.uris[URI_FALDO_PREFIX] contains the URI prefix for FALDO. */
#define URI_ONTOLOGY_PREFIX      0
#define URI_RDF_PREFIX           1
#define URI_RDFS_PREFIX          2
#define URI_XSD_PREFIX           3
#define URI_FALDO_PREFIX         4
#define URI_VCF                  5
#define URI_HG19_PREFIX          6
#define URI_HG19_CHR_PREFIX      7
#define URI_VCF_HEADER_PREFIX    8
#define URI_VCF_HEADER_INFO      9
#define URI_VCF_HEADER_GENERIC   10
#define URI_VCF_HEADER_FORMAT    11
#define URI_VCF_HEADER_FILTER    12
#define URI_VCF_HEADER_ALT       13
#define URI_VCF_HEADER_CONTIG    14
#define URI_VCF_SAMPLE           15
#define URI_VCF_VC_PREFIX        16

/* The following integer is used to determine the size of the contants.
 * Please adjust accordingly when you change the first or the last
 * constant.
 */
#define NUMBER_OF_URIS           (URI_VCF_VC_PREFIX + 1)


#define NODE_VARIANT_CLASS       0

#define NUMBER_OF_NODES          (NODE_VARIANT_CLASS + 1)

/* In addition to URIs and nodes, “datatype property nodes” can contain literal
 * values.  These have a type, which is often described in the xsd namespace.
 */
#define TYPE_STRING              0
#define TYPE_INTEGER             1
#define TYPE_FLOAT               2
#define TYPE_BOOLEAN             3

#define NUMBER_OF_TYPES    (TYPE_BOOLEAN + 1)

/* This struct can be used to make program options available throughout the
 * entire code without needing to pass them around as parameters.  Do not write
 * to these values, other than in the runtime_configuration_init() and
 * ui_process_command_line() functions. */
typedef struct
{
  /* Command-line configurable options. */
  char              *filter;
  char              *keep;
  char              *input_file;
  char              *reference;
  char              *caller;
  int32_t           threads;
  int32_t           jobs_per_thread;
  int32_t           non_unique_variant_counter;
  bool              show_progress_info;

  /* Redland-specifics. */
  librdf_uri        *uris[NUMBER_OF_URIS];
  librdf_uri        *types[NUMBER_OF_TYPES];
  librdf_node       *nodes[NUMBER_OF_NODES];
  librdf_world      *rdf_world;
  librdf_serializer *rdf_serializer;
  librdf_storage    *rdf_storage;
  librdf_model      *rdf_model;

  /* Caching and internal performance optimizing structures. */
  int *info_field_indexes;
  size_t info_field_indexes_len;
  size_t info_field_indexes_blocks;

} RuntimeConfiguration;


/* This is where we can set default values for the program's options. */
RuntimeConfiguration config;

bool runtime_configuration_init (void);
bool runtime_configuration_redland_init (void);
void runtime_configuration_free (void);

char *generate_variant_id (void);
void refresh_model (void);

#endif  /* RUNTIMECONFIGURATION_H */
