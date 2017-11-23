/*
 * Copyright (C) 2017  Roel Janssen <roel@gnu.org>
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

#include "Variant.h"
#include "Origin.h"
#include "helper.h"
#include "RuntimeConfiguration.h"

#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <gcrypt.h>

extern RuntimeConfiguration program_config;

void
gcry_md_write_int32_t (gcry_md_hd_t handler, int32_t number)
{
  char int_str[] = { 0,0,0,0,0,0,0,0,0,0,0 };
  int32_t int_str_len = sprintf (int_str, "%d", number);
  gcry_md_write (handler, int_str, int_str_len);
}

void
gcry_md_write_float (gcry_md_hd_t handler, float number)
{
  int32_t float_str_len = 0;
  char float_str[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  float_str_len = sprintf (float_str, "%04.4f", number);
  gcry_md_write (handler, float_str, float_str_len);
}

char *
variant_name (Variant *v, bcf_hdr_t *vcf_header)
{
  if (v == NULL) return NULL;
  if (v->name[0] != '\0') return v->name;

  if (!isfinite (v->quality))
    v->quality = -1;

  gcry_error_t error;
  gcry_md_hd_t handler = NULL;

  error = gcry_md_open (&handler, GCRY_MD_SHA256, 0);
  if (error)
    {
      fprintf (stderr, "ERROR: %s/%s\n",
               gcry_strsource (error),
               gcry_strerror (error));
      return NULL;
    }

  gcry_md_write_float (handler, v->quality);

  char *start_position_name = faldo_position_name ((FaldoBaseType *)(v->start_position));
  char *end_position_name = faldo_position_name ((FaldoBaseType *)(v->end_position));
  char *cipos_name = faldo_position_name ((FaldoBaseType *)(v->cipos));
  char *ciend_name = faldo_position_name ((FaldoBaseType *)(v->ciend));
  
  unsigned char *binary_hash = NULL;

  /* Provide input for the hash. */
  gcry_md_write (handler, v->reference, strlen (v->reference));
  gcry_md_write (handler, v->alternative, strlen (v->alternative));
  gcry_md_write (handler, start_position_name, v->start_position->name_len);
  gcry_md_write (handler, end_position_name, v->end_position->name_len);
  gcry_md_write (handler, cipos_name, v->cipos->name_len);
  gcry_md_write (handler, ciend_name, v->ciend->name_len);

  if (v->type != NULL)
    gcry_md_write (handler, v->type, v->type_len);

  if (v->id != NULL)
    gcry_md_write (handler, v->id, strlen (v->id));

  if (v->length != 0)
    gcry_md_write_int32_t (handler, v->length);

  if (v->mapq != 0)
    gcry_md_write_int32_t (handler, v->mapq);

  if (v->paired_end_support != 0)
    gcry_md_write_int32_t (handler, v->paired_end_support);

  if (v->split_read_support != 0)
    gcry_md_write_int32_t (handler, v->split_read_support);

  if (v->split_read_consensus_alignment_quality != 0)
    gcry_md_write_float (handler, v->split_read_consensus_alignment_quality);

  if (v->read_count != 0)
    gcry_md_write_int32_t (handler, v->read_count);

  if (v->hq_reference_pairs != 0)
    gcry_md_write_int32_t (handler, v->hq_reference_pairs);

  if (v->hq_variant_pairs != 0)
    gcry_md_write_int32_t (handler, v->hq_variant_pairs);

  if (v->hq_ref_junction_reads != 0)
    gcry_md_write_int32_t (handler, v->hq_ref_junction_reads);

  if (v->hq_var_junction_reads != 0)
    gcry_md_write_int32_t (handler, v->hq_var_junction_reads);

  if (v->is_complex_rearrangement)
    {
      gcry_md_write (handler, (v->is_reversed) ? "T" : "F", 1);
      gcry_md_write (handler, (v->is_left_of_ref) ? "T" : "F", 1);
    }

  /* Concatenate each filter tag for the hash input. */
  int i = 0;
  for (; i < v->filters_len; i++)
    {
      char *name = (char *)vcf_header->id[BCF_DT_ID][v->filters[i]].key;
      gcry_md_write (handler, name, strlen (name));
    }

  binary_hash = gcry_md_read (handler, 0);
  if (!get_pretty_hash (binary_hash, HASH_LENGTH, v->name))
    {
      fprintf (stderr, "ERROR: Couldn't print a hash.\n");
      gcry_md_close (handler);
      return NULL;
    }

  gcry_md_close (handler);
  return v->name;
}

void
variant_print (Variant *v, bcf_hdr_t *vcf_header)
{
  if (v == NULL) return;

  printf ("v:%s a :Variant ;\n", variant_name (v, vcf_header));

  if (v->origin)
    printf ("  :origin o:%s ;\n", hash_Origin (v->origin, true));

  printf ("  :start_position %s:%s ;\n"
          "  :end_position %s:%s ;\n"
          "  :reference \"%s\" ;\n"
          "  :alternative \"%s\" ;\n"
          "  :id \"%s\" ;\n"
          "  :quality %4.2f ",
          faldo_position_prefix ((FaldoBaseType *)(v->start_position)),
          faldo_position_name ((FaldoBaseType *)(v->start_position)),
          faldo_position_prefix ((FaldoBaseType *)(v->end_position)),
          faldo_position_name ((FaldoBaseType *)(v->end_position)),
          v->reference,
          v->alternative,
          v->id,
          v->quality);

  if (v->cipos && v->cipos->before->position != 0)
    printf (";\n  :confidence_interval_start_position %s:%s",
            faldo_position_prefix ((FaldoBaseType *)(v->cipos)),
            faldo_position_name ((FaldoBaseType *)(v->cipos)));

  if (v->ciend && v->ciend->before->position != 0)
    printf (";\n  :confidence_interval_end_position %s:%s",
            faldo_position_prefix ((FaldoBaseType *)(v->ciend)),
            faldo_position_name ((FaldoBaseType *)(v->ciend)));

  if (v->length != 0)
    printf (";\n  :length %d ", v->length);

  int i = 0;
  for (; i < v->filters_len; i++)
    {
      char *name = (char *)vcf_header->id[BCF_DT_ID][v->filters[i]].key;
      printf (";\n  :filter \"%s\" ", name);
    }

  if (v->is_complex_rearrangement)
    {
      printf (";\n  :hasDirection %s ; :atBreakPointPosition %s ",
              (v->is_reversed) ? ":ReverseComplement" : ":Same",
              (v->is_left_of_ref) ? ":Left" : ":Right");
    }

  if (v->mapq != 0)
    printf (";\n  :mapping_quality %d ", v->mapq);

  if (v->paired_end_support != 0)
    printf (";\n  :paired_end_support %d ", v->paired_end_support);

  if (v->split_read_support != 0)
    printf (";\n  :split_read_support %d ", v->split_read_support);

  if (v->split_read_consensus_alignment_quality != 0)
    printf (";\n  :split_read_consensus_alignment_quality  %4.2f ",
            v->split_read_consensus_alignment_quality);

  if (v->read_count != 0)
    printf (";\n  :read_count %d ", v->read_count);

  if (v->hq_reference_pairs != 0)
    printf (";\n  :high_quality_reference_pairs %d ", v->hq_reference_pairs);

  if (v->hq_variant_pairs != 0)
    printf (";\n  :high_quality_variant_pairs %d ", v->hq_variant_pairs);

  if (v->hq_ref_junction_reads != 0)
    printf (";\n  :high_quality_reference_junction_reads %d ",
            v->hq_ref_junction_reads);

  if (v->hq_var_junction_reads != 0)
    printf (";\n  :high_quality_variant_junction_reads  %d ",
            v->hq_var_junction_reads);

  if (v->type_len > 0 && v->type)
    {
      char type[v->type_len + 1];
      memset (type, '\0', v->type_len + 1);
      memcpy (type, v->type, v->type_len);

      printf (";\n  :type \"%s\" .\n\n", type);
    }
  else
    printf (".\n\n");
}

void
variant_initialize (Variant *v, VariantType type)
{
  if (v == NULL) return;
  v->_obj_type = type,
  v->origin = NULL;
  v->start_position = NULL;
  v->end_position = NULL;
  v->cipos = NULL;
  v->ciend = NULL;
  v->quality = 0.0;
  v->reference = NULL;
  v->alternative = NULL;
  v->id = NULL;
  v->filter = NULL;
  v->type = NULL;
  v->type_len = 0;
  v->length = 0;
  v->mapq = 0;
  v->paired_end_support = 0;
  v->split_read_support = 0;
  v->split_read_consensus_alignment_quality = 0;
  v->read_count = 0;
  v->hq_reference_pairs = 0;
  v->hq_variant_pairs = 0;
  v->hq_ref_junction_reads = 0;
  v->hq_var_junction_reads = 0;
  v->is_complex_rearrangement = false;
  v->is_reversed = false;
  v->is_left_of_ref = false;
  memset (v->name, 0, 65);
  v->filters_len = 0;
  v->filters = NULL;
}

void
variant_reset (Variant *v)
{
  if (v == NULL) return;
  variant_initialize (v, v->_obj_type);
}

bool
variant_gather_data (Variant *variant, bcf_hdr_t *header, bcf1_t *buffer)
{
  if (variant == NULL || header == NULL || buffer == NULL ||
      buffer->d.allele == NULL)
    return false;

  /* If the allele information is still missing after unpacking the buffer,
   * we will end up without REF information.  So, let's skip such records. */
  if (buffer->d.allele == NULL)
    {
      fprintf (stderr, "# Skipping record because of missing allele information.\n");
      return false;
    }

  /* TODO: Only the first alternative allele is considered.  When multiple
   * alleles are specified, we need to provide variants for each allele. */
  variant->reference = buffer->d.allele[0];
  variant->alternative = buffer->d.allele[1];
  variant->id = buffer->d.id;
  variant->quality = buffer->qual;

  bcf_unpack (buffer, BCF_UN_FLT);
  variant->filters_len = buffer->d.n_flt;
  variant->filters = buffer->d.flt;

  return true;
}
