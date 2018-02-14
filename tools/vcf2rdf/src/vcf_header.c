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

/* NOTE TO SELF: 
 * We could treat the VCF header as the “ontology” for the variant calls
 * that are about to be described.
 */

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

  librdf_node *vcf_header_item;
  librdf_node *rdf_type;
  librdf_node *origin_type;
  librdf_node *header_generic_type;
  librdf_node *header_info_type;
  librdf_node *header_filter_type;
  librdf_node *header_alt_type;
  librdf_node *header_format_type;
  librdf_node *header_contig_type;
  
  vcf_header_item     = new_node_from_uri (config.uris[URI_VCF_HEADER]);
  rdf_type            = new_node (config.uris[URI_RDF], "type");
  origin_type         = new_node_from_uri (config.uris[URI_VCF_ORIGIN]);
  header_generic_type = new_node_from_uri (config.uris[URI_VCF_HEADER_GENERIC]);
  header_info_type    = new_node_from_uri (config.uris[URI_VCF_HEADER_INFO]);
  header_filter_type  = new_node_from_uri (config.uris[URI_VCF_HEADER_FILTER]);
  header_alt_type     = new_node_from_uri (config.uris[URI_VCF_HEADER_ALT]);
  header_format_type  = new_node_from_uri (config.uris[URI_VCF_HEADER_FORMAT]);
  header_contig_type  = new_node_from_uri (config.uris[URI_VCF_HEADER_CONTIG]);

  librdf_node *self, *key_type, *value_type;
  self       = NULL;
  key_type   = new_node (config.uris[URI_VCF_HEADER], "GenericKey");
  value_type = new_node (config.uris[URI_VCF_HEADER], "GenericValue");

  librdf_node *id, *number, *type, *description, *length, *assembly;
  id          = new_node (config.uris[URI_VCF_HEADER], "Identifier");
  number      = new_node (config.uris[URI_VCF_HEADER], "Number");
  type        = new_node (config.uris[URI_VCF_HEADER], "Type");
  description = new_node (config.uris[URI_VCF_HEADER], "Description");
  length      = new_node (config.uris[URI_VCF_HEADER], "Length");
  assembly    = new_node (config.uris[URI_VCF_HEADER], "Assembly");

  int32_t index = 0;
  char index_str[64];
  for (; index < vcf_header->nhrec; index++)
    {
      memset (index_str, 64, '\0');
      int32_t index_str_len = snprintf (index_str, 64, "h%d", index);
      if (index_str_len < 0 || index_str_len > 64)
        {
          ui_print_memory_error (config.input_file);
          break;
        }

      self = new_node (config.uris[URI_GRAPH_LOCATION], index_str);
      if (!self)
        {
          ui_print_memory_error (config.input_file);
          break;
        }

      /* Add a reference to the 'origin'. */
      add_triplet (copy (self), copy (origin_type), copy (origin));

      /* Add a generic type specification for 'self'. */
      add_triplet (copy (self), copy (rdf_type), copy (vcf_header_item));

      /* Handle simple key-value fields.
       * ------------------------------------------------------------------- */
      if (vcf_header->hrec[index]->value)
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_generic_type));
          add_literal (copy (self), copy (key_type),
                       vcf_header->hrec[index]->key,
                       config.types[TYPE_STRING]);
          add_literal (copy (self), copy (value_type),
                       vcf_header->hrec[index]->value,
                       config.types[TYPE_STRING]);
        }

      /* Handle INFO fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[index]->key, "INFO"))
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_info_type));

          int32_t j;
          for (j = 0; j < vcf_header->hrec[index]->nkeys; j++)
            {
              char *key = vcf_header->hrec[index]->keys[j];
              char *value = vcf_header->hrec[index]->vals[j];
              if (!key || !value) break;

              if (!strcmp (key, "ID"))
                add_literal (copy (self), copy (id), value,
                             config.types[TYPE_STRING]);

              else if (!strcmp (key, "Number"))
                add_literal (copy (self), copy (number), value,
                             config.types[TYPE_STRING]);

              else if (!strcmp (key, "Type")) 
                add_literal (copy (self), copy (type), value,
                             config.types[TYPE_STRING]);

              else if (!strcmp (key, "Description"))
                add_literal (copy (self), copy (description), value,
                             config.types[TYPE_STRING]);
            }
        }

      /* Handle FILTER fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[index]->key, "FILTER"))
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_filter_type));

          int32_t j;
          for (j = 0; j < vcf_header->hrec[index]->nkeys; j++)
            {
              char *key = vcf_header->hrec[index]->keys[j];
              char *value = vcf_header->hrec[index]->vals[j];
              if (!key || !value) break;

              if (!strcmp (key, "ID"))
                add_literal (copy (self), copy (id), value,
                             config.types[TYPE_STRING]);
              else if (!strcmp (key, "Description"))
                add_literal (copy (self), copy (description), value,
                             config.types[TYPE_STRING]);
            }
        }

      /* Handle ALT fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[index]->key, "ALT"))
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_alt_type));

          int32_t j;
          for (j = 0; j < vcf_header->hrec[index]->nkeys; j++)
            {
              char *key = vcf_header->hrec[index]->keys[j];
              char *value = vcf_header->hrec[index]->vals[j];
              if (!key || !value) break;

              if (!strcmp (key, "ID"))
                add_literal (copy (self), copy (id), value,
                             config.types[TYPE_STRING]);
              else if (!strcmp (key, "Description"))
                add_literal (copy (self), copy (description), value,
                             config.types[TYPE_STRING]);
            }
        }

      /* Handle FORMAT fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[index]->key, "FORMAT"))
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_format_type));

          int32_t j;
          for (j = 0; j < vcf_header->hrec[index]->nkeys; j++)
            {
              char *key = vcf_header->hrec[index]->keys[j];
              char *value = vcf_header->hrec[index]->vals[j];
              if (!key || !value) break;

              if (!strcmp (key, "ID"))
                add_literal (copy (self), copy (id), value,
                             config.types[TYPE_STRING]);

              else if (!strcmp (key, "Number"))
                add_literal (copy (self), copy (number), value,
                             config.types[TYPE_STRING]);

              else if (!strcmp (key, "Type")) 
                add_literal (copy (self), copy (type), value,
                             config.types[TYPE_STRING]);

              else if (!strcmp (key, "Description"))
                add_literal (copy (self), copy (description), value,
                             config.types[TYPE_STRING]);
            }
        }

      /* Handle 'contig' fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[index]->key, "contig"))
        {
          add_triplet (copy (self), copy (rdf_type), copy (header_contig_type));

          int32_t j;
          for (j = 0; j < vcf_header->hrec[index]->nkeys; j++)
            {
              char *key = vcf_header->hrec[index]->keys[j];
              char *value = vcf_header->hrec[index]->vals[j];
              if (!key || !value) break;

              if (!strcmp (key, "ID"))
                add_literal (copy (self), copy (id), value,
                             config.types[TYPE_STRING]);

              else if (!strcmp (key, "length"))
                add_literal (copy (self), copy (length), value,
                             config.types[TYPE_INTEGER]);

              else if (!strcmp (key, "assembly")) 
                add_literal (copy (self), copy (assembly), value,
                             config.types[TYPE_STRING]);
            }
        }
      else
        fprintf (stderr, "Error: Encountered an unknown header item '%s'.\n",
                 vcf_header->hrec[index]->key);

      librdf_free_node (self);
      self = NULL;
    }

  librdf_free_node (key_type);
  librdf_free_node (value_type);
  librdf_free_node (vcf_header_item);
  librdf_free_node (rdf_type);
  librdf_free_node (origin_type);
  librdf_free_node (header_generic_type);
  librdf_free_node (header_info_type);
  librdf_free_node (header_filter_type);
  librdf_free_node (header_alt_type);
  librdf_free_node (header_format_type);
  librdf_free_node (header_contig_type);

  librdf_free_node (id);
  librdf_free_node (number);
  librdf_free_node (type);
  librdf_free_node (description);
  librdf_free_node (length);
  librdf_free_node (assembly);
}
