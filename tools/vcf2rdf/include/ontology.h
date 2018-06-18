/* Copyright (C) 2018  Roel Janssen <roel@gnu.org>
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

#ifndef ONTOLOGY_H
#define ONTOLOGY_H

#include <stdbool.h>
#include <stdint.h>
#include <raptor2.h>

/* These string constants can be used to concatenate strings at compile-time. */
#define URI_W3            "http://www.w3.org"
#define URI_BIOSEMANTICS  "http://rdf.biosemantics.org"
#define URI_ASSEMBLIES    URI_BIOSEMANTICS "/data/genomeassemblies"
#define URI_ONTOLOGY      "http://rdf.umcutrecht.nl/vcf2rdf"

typedef enum
{
  PREFIX_BASE = 0,
  PREFIX_SAMPLE,
  PREFIX_VCF_HEADER,
  PREFIX_VCF_HEADER_INFO,
  PREFIX_VCF_HEADER_FORMAT,
  PREFIX_VCF_HEADER_FILTER,
  PREFIX_VCF_HEADER_ALT,
  PREFIX_VCF_HEADER_CONTIG,
  PREFIX_VARIANT_CALL,
  PREFIX_ORIGIN,
  PREFIX_RDF,
  PREFIX_RDFS,
  PREFIX_OWL,
  PREFIX_XSD,
  PREFIX_FALDO,
  PREFIX_HG19,
  PREFIX_HG19_CHR
} ontology_prefix;

typedef enum
{
  CLASS_RDF_TYPE = 0,
  CLASS_ORIGIN,
  CLASS_VCF_HEADER,
  CLASS_VCF_HEADER_INFO,
  CLASS_VCF_HEADER_FORMAT,
  CLASS_VCF_HEADER_FILTER,
  CLASS_VCF_HEADER_ALT,
  CLASS_VCF_HEADER_CONTIG,
  CLASS_SAMPLE,
  CLASS_VARIANT_CALL,
  CLASS_HETEROZYGOUS,
  CLASS_MULTIZYGOUS,
  CLASS_NULLIZYGOUS,
  CLASS_HOMOZYGOUS,
  CLASS_HOMOZYGOUS_REFERENCE,
  CLASS_HOMOZYGOUS_ALTERNATIVE,
  CLASS_XSD_STRING,
  CLASS_XSD_INTEGER,
  CLASS_XSD_FLOAT,
  CLASS_XSD_BOOLEAN
} ontology_class;

typedef struct
{
  raptor_term **classes;
  raptor_uri  **prefixes;
  raptor_uri **xsds;
  int32_t     classes_length;
  int32_t     prefixes_length;
  int32_t     xsds_length;
} ontology_t;

typedef enum
{
  XSD_STRING = 0,
  XSD_INTEGER,
  XSD_FLOAT,
  XSD_BOOLEAN
} xsd_type;

bool ontology_init (ontology_t **ontology_ptr);
void ontology_free (ontology_t *ontology);

/* The following marcros can be used to construct terms (nodes) and URIs.
 * These assume 'config.raptor_world', 'config.uris', 'config.ontology',
 * and 'config.raptor_serializer' exist and have been initialized.
 */
#define uri(index, suffix)                                      \
  raptor_new_uri_relative_to_base (config.raptor_world,         \
                                   config.uris[index],          \
                                   str)

/* TODO: Possibly wrap this in raptor_term_copy. */
#define class(index)                                            \
  raptor_term_copy (config.ontology->classes[index])

#define term(index, suffix)                                     \
  raptor_new_term_from_uri                                      \
  (config.raptor_world,                                         \
    raptor_new_uri_relative_to_base                             \
    (config.raptor_world,                                       \
     config.ontology->prefixes[index],                          \
     (unsigned char *)suffix))

#define literal(str, datatype)                                  \
  raptor_new_term_from_literal                                  \
  (config.raptor_world, (unsigned char *)str,                   \
   config.ontology->xsds[datatype],                             \
   NULL)

#define register_statement(stmt)                                \
  raptor_serializer_serialize_statement                         \
  (config.raptor_serializer, stmt);                             \
  raptor_free_statement (stmt)


#endif  /* ONTOLOGY_H */
