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
  librdf_node *rdf_type    = new_node (config.uris[URI_RDF], "type");
  librdf_node *origin_type = new_node_from_uri (config.uris[URI_VCF_ORIGIN]);
  librdf_node *self        = NULL;

  self = new_node (config.uris[URI_GRAPH_LOCATION], buffer->d.id);
  add_triplet (copy (self), copy (origin_type), copy (origin));
  add_triplet (copy (self), copy (rdf_type),
               new_node_from_uri (config.uris[URI_VCF_VARIANT]));

  /* Add position information
   * ------------------------------------------------------------------------ */
  char *chromosome = (char *)bcf_seqname (header, buffer);

  /* HTSlib uses 0-based positions, while in the VCF 1-based position are used.
   * Therefore we need to add one to the position here. */
  uint32_t position = buffer->pos + 1;

  /* Add the standard fields.
   * Default fields: ID, CHROM, POS, REF, ALT, QUAL, FILTER, INFO, FORMAT.
   * ------------------------------------------------------------------------ */
  add_literal (copy (self),
               new_node (config.uris[URI_VCF_VARIANT_CALL], "CHROM"),
               chromosome,
               config.types[TYPE_STRING]);

  add_literal (copy (self),
               new_node (config.uris[URI_VCF_VARIANT_CALL], "ID"),
               (char *)buffer->d.id,
               config.types[TYPE_STRING]);

  char position_str[10];
  snprintf (position_str, 10, "%u", position);

  add_literal (copy (self),
               new_node (config.uris[URI_VCF_VARIANT_CALL], "POS"),
               position_str,
               config.types[TYPE_INTEGER]);

  add_literal (copy (self),
               new_node (config.uris[URI_VCF_VARIANT_CALL], "REF"),
               buffer->d.allele[0],
               config.types[TYPE_STRING]);

  add_literal (copy (self),
               new_node (config.uris[URI_VCF_VARIANT_CALL], "ALT"),
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
                   new_node (config.uris[URI_VCF_VARIANT_CALL], "QUAL"),
                   qual_str,
                   config.types[TYPE_FLOAT]);
    }

  /* Process filter fields.
   * ------------------------------------------------------------------------ */
  bcf_unpack (buffer, BCF_UN_FLT);
  int filter_index = 0;
  for (; filter_index < buffer->d.n_flt; filter_index++)
    add_literal (copy (self),
                 new_node (config.uris[URI_VCF_VARIANT_CALL], "FILTER"),
                 header->id[BCF_DT_ID][buffer->d.flt[filter_index]].key,
                 config.types[TYPE_STRING]);

  librdf_free_node (rdf_type);
  librdf_free_node (origin_type);
  librdf_free_node (self);
}
