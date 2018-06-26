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
#include <raptor2.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

void
get_field_identity (bcf_hdr_t *header, int32_t index, char **id_str, int32_t *type)
{
  int32_t j = 0;
  for (; j < header->hrec[index]->nkeys; j++)
    {
      char *key   = header->hrec[index]->keys[j];
      char *value = header->hrec[index]->vals[j];
      if (!key || !value) continue;
      if (!strcmp (key, "ID"))
        *id_str = value;
      else if (!strcmp (key, "Type"))
        *type = (!strcmp (value, "Integer")) ? XSD_INTEGER
              : (!strcmp (value, "Float"))   ? XSD_FLOAT
              : (!strcmp (value, "String"))  ? XSD_STRING
              : (!strcmp (value, "Flag"))    ? XSD_BOOLEAN
              : -1;

      if (*id_str && *type != -1) break;
    }
}

void
process_variant (bcf_hdr_t *header, bcf1_t *buffer, const unsigned char *origin)
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
  uint32_t number_of_samples = bcf_hdr_nsamples (header);
  int32_t sample_index = 0;
  raptor_term *origin_type = term (PREFIX_BASE, "originatedFrom");
  if (!origin_type || !origin)
    {
      ui_print_redland_error ();
      return;
    }

  for (; sample_index < number_of_samples; sample_index++)
    {
      /* Create 'generic' nodes and URIs.
       * -------------------------------------------------------------------- */
      raptor_term *self        = NULL;
      raptor_statement *stmt   = NULL;
      char *variant_id         = NULL;

      if (! generate_variant_id (origin, config.variant_id_buf))
        ui_print_general_memory_error ();
      else
        variant_id = config.variant_id_buf;

      self = term (PREFIX_BASE, variant_id);
      if (!self)
        {
          ui_print_redland_error ();
          return;
        }

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (self);
      stmt->predicate = raptor_term_copy (origin_type);
      stmt->object    = term (PREFIX_BASE, (char *)origin);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (self);
      stmt->predicate = term (PREFIX_BASE, "sample");
      stmt->object    = term (PREFIX_SAMPLE, header->samples[sample_index]);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (self);
      stmt->predicate = class (CLASS_RDF_TYPE);
      stmt->object    = class (CLASS_VARIANT_CALL);
      register_statement (stmt);

      /* The original variant ID should be preserved.  Unfortunately, it is
       * not guaranteed to be unique, so we can't use it as an identifier.
       *
       * Furthermore, we noticed that many variant IDs are simply a single dot,
       * so to avoid duplicated entries for dots, we filter those out. */
      if (buffer->d.id[0] != '.')
        {
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = raptor_term_copy (self);
          stmt->predicate = term (PREFIX_BASE, "variantId");
          stmt->object    = literal (buffer->d.id, XSD_STRING);
          register_statement (stmt);
        }

      /* Add position information
       * -------------------------------------------------------------------- */

      /* We would usually do 'chromosome = bcf_seqname (header, buffer)', but
       * the function call overhead is worth avoiding.  So we just directly access
       * the field containing the chromosome identifier instead. */
      char *chromosome = (char *)header->id[BCF_DT_CTG][buffer->rid].key;
      size_t chromosome_len = strlen (chromosome);

      /* HTSlib uses 0-based positions, while in the VCF 1-based position are used.
       * Therefore we need to add one to the position here. */
      uint32_t position = buffer->pos + 1;

      /* Add the standard fields.
       * Default fields: ID, CHROM, POS, REF, ALT, QUAL, FILTER, INFO, FORMAT.
       * ------------------------------------------------------------------------ */
      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (self);
      stmt->predicate = term (PREFIX_FALDO, "#reference");

      /* The chromosome can be a contig name or the usual 1..MT.
       * The usual ones are prefixed by "chr" in the ontology we use
       * to describe a chromosome. */
      char chr_buffer[16];
      if (chromosome_len < 3)
        snprintf (chr_buffer, 16, "#chr%s", chromosome);
      else
        snprintf (chr_buffer, 16, "#%s", chromosome);

      stmt->object    = term (PREFIX_HG19, chr_buffer);
      register_statement (stmt);

      snprintf (config.number_buffer, 32, "%u", position);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (self);
      stmt->predicate = term (PREFIX_FALDO, "#position");
      stmt->object    = literal (config.number_buffer, XSD_INTEGER);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (self);
      stmt->predicate = term (PREFIX_VARIANT_CALL, "REF");
      stmt->object    = term (PREFIX_SEQUENCE, buffer->d.allele[0]);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (self);
      stmt->predicate = term (PREFIX_VARIANT_CALL, "ALT");
      stmt->object    = term (PREFIX_SEQUENCE, buffer->d.allele[1]);
      register_statement (stmt);

      /* The QUAL indicator "." means that the QUAL value is missing or unknown.
       * In such a case we skip the entire triplet.  This behavior needs to be
       * documented as such, so that users don't forget to treat it as an optional
       * field. */
      if (isfinite (buffer->qual))
        {
          snprintf (config.number_buffer, 32, "%4.6f", buffer->qual);

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = raptor_term_copy (self);
          stmt->predicate = term (PREFIX_VARIANT_CALL, "QUAL");
          stmt->object    = literal (config.number_buffer, XSD_FLOAT);

          register_statement (stmt);
        }

      /* Process filter fields.
       * ------------------------------------------------------------------------ */
      bcf_unpack (buffer, BCF_UN_FLT);
      int filter_index = 0;
      for (; filter_index < buffer->d.n_flt; filter_index++)
        {
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = raptor_term_copy (self);
          stmt->predicate = term (PREFIX_VARIANT_CALL, "FILTER");
          stmt->object    = term (PREFIX_BASE,
                                  (char *)(header->id[BCF_DT_ID][buffer->d.flt[filter_index]].key));
          register_statement (stmt);
        }

      /* Process INFO fields.
       * ------------------------------------------------------------------------- */

      char *id_str           = NULL;
      void *value            = NULL;
      int32_t state          = 0;
      int32_t type           = -1;
      int32_t value_len      = 0;
      int32_t index          = 0;
      uint32_t i             = 0;

      for (i = 0; i < config.info_field_indexes_len; i++)
        {
          id_str    = NULL;
          type      = -1;
          value     = NULL;
          value_len = 0;
          index     = config.info_field_indexes[i];

          get_field_identity (header, index, &id_str, &type);

          if (!id_str || type == -1)
            goto clean_up_iteration;

          state = bcf_get_info_values (header, buffer, id_str, &value, &value_len, type);
          if (!value || state < 0)
            goto clean_up_iteration;

          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = raptor_term_copy (self);
          stmt->predicate = term (PREFIX_VCF_HEADER_INFO, id_str);

          if (type == XSD_INTEGER)
            {
              snprintf (config.number_buffer, 32, "%d", *((int32_t *)value));
              stmt->object = literal (config.number_buffer, XSD_INTEGER);
            }
          else if (type == XSD_FLOAT)
            {
              snprintf (config.number_buffer, 32, "%f", *((float *)value));
              stmt->object = literal (config.number_buffer, XSD_FLOAT);
            }
          else if (type == XSD_STRING)
            stmt->object = literal ((char *)value, XSD_STRING);
          else if (type == XSD_BOOLEAN && state == 1)
            stmt->object = literal ("true", XSD_BOOLEAN);

          if (stmt->subject != NULL && stmt->predicate != NULL && stmt->object != NULL)
            register_statement (stmt);

        clean_up_iteration:
          free (value);
          value = NULL;
        }

      /* Process FORMAT fields.
       * ------------------------------------------------------------------------- */
      for (i = 0; i < config.format_field_indexes_len; i++)
        {
          id_str    = NULL;
          type      = -1;
          value     = NULL;
          value_len = 0;

          get_field_identity (header,
                              config.format_field_indexes[i],
                              &id_str,
                              &type);

          if (!id_str || type == -1)
            continue;

          if (!strcmp (id_str, "GT"))
            {
              char **dst = NULL;
              int32_t ndst = 0;
              int32_t gt = bcf_get_genotypes (header, buffer, &dst, &ndst);
              int32_t ploidy = gt / number_of_samples;
              int32_t *ptr = (int32_t *)dst + sample_index * ploidy;
              int32_t *genotypes = calloc (ploidy, sizeof (int32_t));

              int32_t k;
              for (k = 0; k < ploidy; k++)
                genotypes[k] = (bcf_gt_is_missing (ptr[k]))
                  ? -1
                  : bcf_gt_allele (ptr[k]);

              int32_t genotype_class = -1;

              if (ploidy == 2)
                genotype_class = (genotypes[0] == 0 && genotypes[1] == 0)
                  ? CLASS_HOMOZYGOUS_REFERENCE
                  : (genotypes[0] == 1 && genotypes[1] == 1)
                  ? CLASS_HOMOZYGOUS_ALTERNATIVE
                  : CLASS_HETEROZYGOUS;

              else if (ploidy == 1)
                genotype_class = CLASS_HOMOZYGOUS;
              else if (ploidy > 2)
                genotype_class = CLASS_MULTIZYGOUS;
              else if (ploidy == 0)
                genotype_class = CLASS_NULLIZYGOUS;

              free (genotypes);
              free (dst);

              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = raptor_term_copy (self);
              stmt->predicate = term (PREFIX_RDF, "#type");
              stmt->object    = class (genotype_class);
              register_statement (stmt);
            }
          else
            {
              state = bcf_get_format_values (header, buffer, id_str, &value, &value_len, type);
              if (!value || state < 0)
                goto clean_format_iteration;

              stmt = raptor_new_statement (config.raptor_world);
              stmt->subject   = raptor_term_copy (self);
              stmt->predicate = term (PREFIX_VCF_HEADER_FORMAT, id_str);

              if (type == XSD_INTEGER)
                {
                  snprintf (config.number_buffer, 32, "%d", *((int32_t *)value));
                  stmt->object = literal (config.number_buffer, XSD_INTEGER);
                }
              else if (type == XSD_FLOAT)
                {
                  snprintf (config.number_buffer, 32, "%f", *((float *)value));
                  stmt->object = literal (config.number_buffer, XSD_FLOAT);
                }
              else if (type == XSD_STRING)
                stmt->object = literal ((char *)value, XSD_STRING);
              else if (type == XSD_BOOLEAN && state == 1)
                stmt->object = literal ("true", XSD_BOOLEAN);

              if (stmt->subject != NULL && stmt->predicate != NULL && stmt->object != NULL)
                register_statement (stmt);

            clean_format_iteration:
              free (value);
              value = NULL;

            }
        }

      raptor_free_term (self);
    }

  raptor_free_term (origin_type);
}
