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

#include "vcf_header.h"
#include "runtime_configuration.h"
#include "helper.h"
#include "ui.h"

#include <stdio.h>
#include <librdf.h>

void
process_header_item (bcf_hdr_t   *vcf_header,
                     librdf_node *origin,
                     librdf_node *self,
                     librdf_uri  *prefix,
                     int32_t index)
{
  int32_t j = 0;
  for (; j < vcf_header->hrec[index]->nkeys; j++)
    {
      char *key   = vcf_header->hrec[index]->keys[j];
      char *value = vcf_header->hrec[index]->vals[j];

      if (!key || !value ||
          /* It seems that htslib adds an “IDX” key/value pair at the end
           * to keep track of the order of the header items.  We don't
           * want that property to leak into the RDF. */
          !strcmp (key, "IDX"))
        continue;

      if (strcmp (key, "ID"))
        add_literal (copy (self),
                     new_node (prefix, key),
                     value,
                     config.types[TYPE_STRING]);
    }
}

void
process_header (bcf_hdr_t *vcf_header, librdf_node *origin)
{
  if (!vcf_header || !origin) return;

  /* GENERAL NODES
   * --------------------------------------------------------------------------
   * The following nodes are used multiple times in the processing loop.  Once
   * a node is added to the model (via a statement), its memory is managed by
   * the model.  This means that we can't add the node to more than one
   * statement.  Instead we have to clone the node first, so that they live in
   * two disjoint memory locations.
   *
   * If we are looking for performance enhancements, we could change this in
   * librdf.
   */

  librdf_node *rdf_type;
  librdf_node *origin_type;
  librdf_node *self   = NULL;
  librdf_uri  *prefix = NULL;
  librdf_node *class  = NULL;
  rdf_type            = new_node (config.uris[URI_RDF_PREFIX], "type");
  origin_type         = new_node (config.uris[URI_ONTOLOGY_PREFIX],
                                  "originatedFrom");

  /* Register samples.
   * ----------------------------------------------------------------------- */
  int32_t number_of_samples = bcf_hdr_nsamples (vcf_header);
  int32_t index             = 0;

  for (; index < number_of_samples; index++)
    {
      librdf_node *sample = new_node (config.uris[URI_SAMPLE_PREFIX],
                                      vcf_header->samples[index]);

      add_triplet (copy (sample),
                   copy (rdf_type),
                   copy (config.nodes[NODE_SAMPLE_CLASS]));

      add_triplet (copy (sample),
                   new_node (config.uris[URI_ONTOLOGY_PREFIX], "foundIn"),
                   copy (origin));

      librdf_free_node (sample);
    }

  /* Process header fields.
   * ----------------------------------------------------------------------- */

  char* identifier    = NULL;
  for (index = 0; index < vcf_header->nhrec; index++)
    {
      if (vcf_header->hrec[index]->nkeys > 0)
        {
          char *key   = vcf_header->hrec[index]->keys[0];
          char *value = vcf_header->hrec[index]->vals[0];
          if (key && value)
            {
              if (!strcmp (key, "ID"))
                identifier = value;
              else
                identifier = key;
            }
        }
      else
        identifier = vcf_header->hrec[index]->key;

      if (!identifier)
        continue;

      /* Handle simple key-value fields.
       * ------------------------------------------------------------------- */
      if (vcf_header->hrec[index]->value)
        {
          self = new_node (config.uris[URI_VCF_HEADER_GENERIC_PREFIX], identifier);
          add_triplet (copy (self),
                       copy (rdf_type),
                       copy (config.nodes[NODE_VCF_HEADER_GENERIC_CLASS]));

          add_literal (copy (self),
                       new_node (config.uris[URI_VCF_HEADER_GENERIC_PREFIX],
                                 vcf_header->hrec[index]->key),
                       vcf_header->hrec[index]->value,
                       config.types[TYPE_STRING]);
        }

      /* Handle other fields.
       * ------------------------------------------------------------------- */
      else if (vcf_header->hrec[index]->type == BCF_HL_INFO)
        {
          prefix = config.uris[URI_VCF_HEADER_INFO_PREFIX];
          class  = config.nodes[NODE_VCF_HEADER_INFO_CLASS];

          /* Cache the indexes of INFO fields.
           * ---------------------------------------------------------------- */

          /* Resize block when needed. */
          if ((config.info_field_indexes_blocks * 1024) <=
              config.info_field_indexes_len)
            {
              config.info_field_indexes_blocks++;
              config.info_field_indexes = realloc (config.info_field_indexes,
                                                   config.info_field_indexes_blocks
                                                   * 1024 * sizeof (int *));
            }

          /* Store the index in the cache. */
          config.info_field_indexes[config.info_field_indexes_len] = index;
          config.info_field_indexes_len++;
        }

      else if (vcf_header->hrec[index]->type == BCF_HL_FLT)
        {
          prefix = config.uris[URI_VCF_HEADER_FILTER_PREFIX];
          class  = config.nodes[NODE_VCF_HEADER_FILTER_CLASS];
        }
      else if (vcf_header->hrec[index]->type == BCF_HL_FMT)
        {
          prefix = config.uris[URI_VCF_HEADER_FORMAT_PREFIX];
          class  = config.nodes[NODE_VCF_HEADER_FORMAT_CLASS];

          /* Cache the indexes of FORMAT fields.
           * ---------------------------------------------------------------- */

          /* Resize block when needed. */
          if ((config.format_field_indexes_blocks * 1024) <=
              config.format_field_indexes_len)
            {
              config.format_field_indexes_blocks++;
              config.format_field_indexes = realloc (config.format_field_indexes,
                                                   config.format_field_indexes_blocks
                                                   * 1024 * sizeof (int *));
            }

          /* Store the index in the cache. */
          config.format_field_indexes[config.format_field_indexes_len] = index;
          config.format_field_indexes_len++;
        }
      else if (vcf_header->hrec[index]->type == BCF_HL_CTG)
        {
          prefix = config.uris[URI_VCF_HEADER_CONTIG_PREFIX];
          class  = config.nodes[NODE_VCF_HEADER_CONTIG_CLASS];
        }
      else if (!strcmp (vcf_header->hrec[index]->key, "ALT"))
        {
          prefix = config.uris[URI_VCF_HEADER_ALT_PREFIX];
          class  = config.nodes[NODE_VCF_HEADER_ALT_CLASS];
        }
      else
        {
          prefix = config.uris[URI_VCF_HEADER_GENERIC_PREFIX];
          class  = config.nodes[NODE_VCF_HEADER_GENERIC_CLASS];
        }

      if (prefix != NULL && class != NULL)
        {
          self = new_node (prefix, identifier);
          add_triplet (copy (self), copy (rdf_type), copy (class));
          process_header_item (vcf_header, origin, self, prefix, index);
        }

      /* Add a reference to the 'origin'. */
      add_triplet (copy (self), copy (origin_type), copy (origin));

      librdf_free_node (self);
      self   = NULL;
      prefix = NULL;
      class  = NULL;
    }

  librdf_free_node (rdf_type);
  librdf_free_node (origin_type);
}
