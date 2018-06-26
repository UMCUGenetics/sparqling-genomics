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
#include <htslib/vcf.h>

/* These string constants can be used to concatenate strings at compile-time. */
#define URI_W3            "http://www.w3.org"
#define URI_BIOSEMANTICS  "http://rdf.biosemantics.org"
#define URI_ASSEMBLIES    URI_BIOSEMANTICS "/data/genomeassemblies"
#define URI_ONTOLOGY      "http://rdf.umcutrecht.nl/vcf2rdf"

#define STR_PREFIX_BASE                URI_ONTOLOGY "/"
#define STR_PREFIX_SAMPLE              URI_ONTOLOGY "/Sample/"
#define STR_PREFIX_VCF_HEADER          URI_ONTOLOGY "/VcfHeaderItem/"
#define STR_PREFIX_VCF_HEADER_INFO     URI_ONTOLOGY "VcfHeaderInfoItem/"
#define STR_PREFIX_VCF_HEADER_FORMAT   URI_ONTOLOGY "VcfHeaderFormatItem/"
#define STR_PREFIX_VCF_HEADER_FILTER   URI_ONTOLOGY "VcfHeaderFilterItem/"
#define STR_PREFIX_VCF_HEADER_ALT      URI_ONTOLOGY "VcfHeaderAltItem/"
#define STR_PREFIX_VCF_HEADER_CONTIG   URI_ONTOLOGY "VcfHeaderContigItem/"
#define STR_PREFIX_VARIANT_CALL        URI_ONTOLOGY "/VariantCall/"
#define STR_PREFIX_SEQUENCE            URI_ONTOLOGY "/Sequence/"
#define STR_PREFIX_ORIGIN              URI_ONTOLOGY "/Origin/"
#define STR_PREFIX_RDF                 URI_W3 "/1999/02/22-rdf-syntax-ns#"
#define STR_PREFIX_RDFS                URI_W3 "/2000/01/rdf-schema#"
#define STR_PREFIX_OWL                 URI_W3 "/2002/07/owl#"
#define STR_PREFIX_XSD                 URI_W3 "/2001/XMLSchema#"
#define STR_PREFIX_FALDO               "http://biohackathon.org/resource/faldo#"
#define STR_PREFIX_HG19                URI_ASSEMBLIES "/hg19#"
#define STR_PREFIX_HG19_CHR            URI_ASSEMBLIES "/hg19#chr"

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
  PREFIX_SEQUENCE,
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
  CLASS_HOMOZYGOUS_ALTERNATIVE
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

#define XSD_STRING              BCF_HT_STR
#define XSD_INTEGER             BCF_HT_INT
#define XSD_FLOAT               BCF_HT_REAL
#define XSD_BOOLEAN             BCF_HT_FLAG

bool ontology_init (ontology_t **ontology_ptr);
void ontology_free (ontology_t *ontology);

raptor_term* term (int32_t index, char *suffix);

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
