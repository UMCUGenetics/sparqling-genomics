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

#include "vcf_variants.h"
#include "runtime_configuration.h"
#include "helper.h"
#include "ui.h"

#include <stdio.h>
#include <htslib/vcf.h>
#include <librdf.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

void
process_variant (bcf_hdr_t *header, bcf1_t *buffer, librdf_node *origin)
{
  if (!header || !buffer || !origin) return;

  /* Handle the program options for leaving out FILTER fields.
   * ------------------------------------------------------------------------ */
  if (config.filter && bcf_has_filter (header, buffer, config.filter) == 1)
    return;

  if (config.keep && bcf_has_filter (header, buffer, config.keep) != 1)
    return;

  /* Unpack up and including the ALT field.
   * ------------------------------------------------------------------------ */
  bcf_unpack (buffer, BCF_UN_STR);

  /* If the allele information is still missing after unpacking the buffer,
   * we will end up without REF information.  Skip these records. */
  if (buffer->d.allele == NULL)
    return;

  /* Create 'generic' nodes and URIs.
   * ------------------------------------------------------------------------ */
  librdf_node *rdf_type    = new_node (config.uris[URI_RDF_PREFIX], "type");
  librdf_node *origin_type = new_node (config.uris[URI_ONTOLOGY_PREFIX], "origin");
  librdf_node *self        = NULL;
  char *variant_id         = NULL;
  bool variant_id_free_p   = false;

  if (buffer->d.id[0] != '.')
    {
      /* In some rare cases, multiple identifiers are stuffed into the ID
       * field, separated by a semicolon.  The code below only takes the
       * first. */
      char *semicolon = strchr (buffer->d.id, ';');
      if (semicolon != NULL)
        {
          variant_id = strdup (buffer->d.id);
          variant_id[semicolon - buffer->d.id] = '\0';
          variant_id_free_p = true;
        }
      else
        variant_id = buffer->d.id;
    }
  else
    {
      variant_id = generate_variant_id ();
      variant_id_free_p = true;
    }

  self = new_node (config.uris[URI_ONTOLOGY_PREFIX], variant_id);

  add_triplet (copy (self), copy (origin_type), copy (origin));
  add_triplet (copy (self), copy (rdf_type),    copy (config.nodes[NODE_VARIANT_CLASS]));

  /* Add position information
   * ------------------------------------------------------------------------ */
  char *chromosome = (char *)bcf_seqname (header, buffer);
  size_t chromosome_len = strlen (chromosome);

  /* HTSlib uses 0-based positions, while in the VCF 1-based position are used.
   * Therefore we need to add one to the position here. */
  uint32_t position = buffer->pos + 1;

  /* Add the standard fields.
   * Default fields: ID, CHROM, POS, REF, ALT, QUAL, FILTER, INFO, FORMAT.
   * ------------------------------------------------------------------------ */
  add_triplet (copy (self),
               new_node (config.uris[URI_FALDO_PREFIX], "reference"),
               /* The chromosome can be a contig name or the usual 1..MT.
                * The usual ones are prefixed by "chr" in the ontology we use
                * to describe a chromosome.  To avoid string copying, we can
                * use the URI_HG19_CHR_PREFIX. */
               (chromosome_len < 3)
                 ? new_node (config.uris[URI_HG19_CHR_PREFIX], chromosome)
                 : new_node (config.uris[URI_HG19_PREFIX], chromosome));

  add_triplet (copy (self),
               new_node (config.uris[URI_VCF_VC_PREFIX], "ID"),
               new_node (config.uris[URI_ONTOLOGY_PREFIX], variant_id));

  char position_str[10];
  snprintf (position_str, 10, "%u", position);

  add_literal (copy (self),
               new_node (config.uris[URI_FALDO_PREFIX], "position"),
               position_str,
               config.types[TYPE_INTEGER]);

  add_literal (copy (self),
               new_node (config.uris[URI_VCF_VC_PREFIX], "REF"),
               buffer->d.allele[0],
               config.types[TYPE_STRING]);

  add_literal (copy (self),
               new_node (config.uris[URI_VCF_VC_PREFIX], "ALT"),
               buffer->d.allele[1],
               config.types[TYPE_STRING]);

  /* The QUAL indicator "." means that the QUAL value is missing or unknown.
   * In such a case we skip the entire triplet.  This behavior needs to be
   * documented as such, so that users don't forget to treat it as an optional
   * field. */
  if (isfinite (buffer->qual))
    {
      char qual_str[9];
      memset (qual_str, '\0', 9);
      snprintf (qual_str, 9, "%4.3f", buffer->qual);

      add_literal (copy (self),
                   new_node (config.uris[URI_VCF_VC_PREFIX], "QUAL"),
                   qual_str,
                   config.types[TYPE_FLOAT]);
    }

  /* Process filter fields.
   * ------------------------------------------------------------------------ */
  bcf_unpack (buffer, BCF_UN_FLT);
  int filter_index = 0;
  for (; filter_index < buffer->d.n_flt; filter_index++)
    add_literal (copy (self),
                 new_node (config.uris[URI_VCF_VC_PREFIX], "FILTER"),
                 header->id[BCF_DT_ID][buffer->d.flt[filter_index]].key,
                 config.types[TYPE_STRING]);

  /* Process INFO fields.
  * ------------------------------------------------------------------------- */
  char *query_string = calloc (512, sizeof (char));
  snprintf (query_string, 512,
            "SELECT DISTINCT ?id ?datatype WHERE { ?subject <%sID> ?id ; "
            "<%sType> ?datatype ;<%stype> <%s> . }",
            librdf_uri_as_string (config.uris[URI_VCF_HEADER_PREFIX]),
            librdf_uri_as_string (config.uris[URI_VCF_HEADER_PREFIX]),
            librdf_uri_as_string (config.uris[URI_RDF_PREFIX]),
            librdf_uri_as_string (config.uris[URI_VCF_HEADER_INFO]));

  librdf_query *query;
  query = librdf_new_query (config.rdf_world, "sparql", NULL, query_string, NULL);

  if (!query)
    ui_print_query_error (query_string);
  else
    {
      librdf_query_results *results = NULL;
      results = librdf_query_execute (query, config.rdf_model);

      if (!results)
        ui_print_query_error (query_string);

      do
        {
          librdf_node *id_node   = NULL;
          librdf_node *type_node = NULL;
          char *id_str           = NULL;
          char *type_str         = NULL;
          void *value            = NULL;
          int32_t value_len      = 0;

          id_node   = librdf_query_results_get_binding_value (results, 0);
          type_node = librdf_query_results_get_binding_value (results, 1);
          if (!id_node || !type_node) continue;

          id_str   = librdf_node_get_literal_value (id_node);
          type_str = librdf_node_get_literal_value (type_node);
          if (!id_str || !type_str) continue;

          char number_buffer[32];
          memset(number_buffer, 32, '\0');

          if (!strcmp (type_str, "Integer"))
            {
              bcf_get_info_int32 (header, buffer, id_str, &value, &value_len);
              if (!value) continue;

              snprintf (number_buffer, 32, "%d", *((int32_t *)value));
              add_literal (copy (self),
                           new_node (config.uris[URI_VCF_HEADER_PREFIX], id_str),
                           number_buffer,
                           config.types[TYPE_INTEGER]);
            }
          else if (!strcmp (type_str, "Float"))
            {
              bcf_get_info_float (header, buffer, id_str, &value, &value_len);
              if (!value) continue;

              snprintf (number_buffer, 32, "%f", *((float *)value));
              add_literal (copy (self),
                           new_node (config.uris[URI_VCF_HEADER_PREFIX], id_str),
                           number_buffer,
                           config.types[TYPE_FLOAT]);
            }
          else if (!strcmp (type_str, "String"))
            {
              int32_t state = bcf_get_info_string (header, buffer, id_str, &value, &value_len);
              if (state >= 0)
                add_literal (copy (self),
                             new_node (config.uris[URI_VCF_HEADER_PREFIX], id_str),
                             (char *)value,
                             config.types[TYPE_STRING]);
            }
          else if (!strcmp (type_str, "Character"))
            {
              fprintf (stderr, "INFO fields containing 'Character', have not been implemented (yet).\n");
            }
          else if (!strcmp (type_str, "Flag"))
            {
              int32_t state = bcf_get_info_flag (header, buffer, id_str, &value, &value_len);
              if (state == 1)
                add_literal (copy (self),
                             new_node (config.uris[URI_VCF_HEADER_PREFIX], id_str),
                             "1",
                             config.types[TYPE_BOOLEAN]);
            }
        }
      while (librdf_query_results_next (results) == 0);
    }

  if (variant_id_free_p) free (variant_id);
  librdf_free_node (rdf_type);
  librdf_free_node (origin_type);
  librdf_free_node (self);
}
