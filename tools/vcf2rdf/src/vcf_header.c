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
  librdf_node *header_generic_type;
  librdf_node *header_info_type;
  librdf_node *header_filter_type;
  librdf_node *header_alt_type;
  librdf_node *header_format_type;
  librdf_node *header_contig_type;

  rdf_type            = new_node (config.uris[URI_RDF_PREFIX], "type");
  origin_type         = new_node (config.uris[URI_ONTOLOGY_PREFIX], "origin");
  header_generic_type = new_node_from_uri (config.uris[URI_VCF_HEADER_GENERIC]);
  header_info_type    = new_node_from_uri (config.uris[URI_VCF_HEADER_INFO]);
  header_filter_type  = new_node_from_uri (config.uris[URI_VCF_HEADER_FILTER]);
  header_alt_type     = new_node_from_uri (config.uris[URI_VCF_HEADER_ALT]);
  header_format_type  = new_node_from_uri (config.uris[URI_VCF_HEADER_FORMAT]);
  header_contig_type  = new_node_from_uri (config.uris[URI_VCF_HEADER_CONTIG]);

  librdf_node *self = NULL;

  int32_t index = 0;
  char* identifier = NULL;

  for (; index < vcf_header->nhrec; index++)
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

      self = new_node (config.uris[URI_VCF_HEADER_PREFIX], identifier);
      if (!self)
        {
          ui_print_memory_error (config.input_file);
          break;
        }

      /* Add a reference to the 'origin'. */
      add_triplet (copy (self), copy (origin_type), copy (origin));

      /* Handle simple key-value fields.
       * ------------------------------------------------------------------- */
      if (vcf_header->hrec[index]->value)
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_generic_type));
          add_literal (copy (self),
                      new_node (config.uris[URI_VCF_HEADER_PREFIX],
                                vcf_header->hrec[index]->key),
                      vcf_header->hrec[index]->value,
                      config.types[TYPE_STRING]);
        }

      /* Handle other fields.
       * ------------------------------------------------------------------- */
      int32_t j;
      for (j = 0; j < vcf_header->hrec[index]->nkeys; j++)
        {
          char *key = vcf_header->hrec[index]->keys[j];
          char *value = vcf_header->hrec[index]->vals[j];
          if (!key || !value ||
              /* It seems that htslib adds an “IDX” key/value pair at the end
               * to keep track of the order of the header items.  We don't
               * want that property to leak into the RDF. */
              !strcmp(vcf_header->hrec[index]->keys[j], "IDX"))
            break;

          if (strcmp (key, "ID"))
            add_literal (copy (self),
                         new_node (config.uris[URI_VCF_HEADER_PREFIX], key),
                         value,
                         config.types[TYPE_STRING]);
        }

      /* Add a specific header type identifier.
       * ------------------------------------------------------------------- */
      if (!strcmp (vcf_header->hrec[index]->key, "INFO"))
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_info_type));

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

      else if (!strcmp (vcf_header->hrec[index]->key, "FILTER"))
        add_triplet (copy (self), copy (rdf_type), copy (header_filter_type));
      else if (!strcmp (vcf_header->hrec[index]->key, "ALT"))
        add_triplet (copy (self), copy (rdf_type), copy (header_alt_type));
      else if (!strcmp (vcf_header->hrec[index]->key, "FORMAT"))
        add_triplet (copy (self), copy (rdf_type), copy (header_format_type));
      else if (!strcmp (vcf_header->hrec[index]->key, "contig"))
        add_triplet (copy (self), copy (rdf_type), copy (header_contig_type));
      else
        add_triplet (copy (self), copy (rdf_type), copy (header_generic_type));

      librdf_free_node (self);
      self = NULL;
    }

  librdf_free_node (rdf_type);
  librdf_free_node (origin_type);
  librdf_free_node (header_generic_type);
  librdf_free_node (header_info_type);
  librdf_free_node (header_filter_type);
  librdf_free_node (header_alt_type);
  librdf_free_node (header_format_type);
  librdf_free_node (header_contig_type);
}
