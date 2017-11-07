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

  /* Avoid dynamic allocation for a speed-up. */
  int32_t quality_strlen = 0;
  char quality_str[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  quality_strlen = sprintf (quality_str, "%04.4f", v->quality);

  unsigned char *binary_hash = NULL;
  char *position_name = faldo_position_name (v->position);

  /* Provide input for the hash. */
  gcry_md_write (handler,
                 position_name,
                 v->position->name_len);

  /* Concatenate each filter tag for the hash input. */
  int i = 0;
  for (; i < v->filters_len; i++)
    {
      char *name = (char *)vcf_header->id[BCF_DT_ID][v->filters[i]].key;
      gcry_md_write (handler, name, strlen (name));
    }

  gcry_md_write (handler, quality_str, quality_strlen);

  if (v->type != NULL)
    gcry_md_write (handler, v->type, v->type_len);

  if (v->id != NULL)
    gcry_md_write (handler, v->id, strlen (v->id));

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

  printf ("  :position %s:%s ;\n  :confidence_interval %s:%s ;\n"
          "  :reference \"%s\" ;\n"
          "  :alternative \"%s\" ;\n"
          "  :id \"%s\" ;\n",
          faldo_position_prefix (v->position),
          faldo_position_name (v->position),
          faldo_position_prefix (v->confidence_interval),
          faldo_position_name (v->confidence_interval),
          v->reference,
          v->alternative,
          v->id);

  int i = 0;
  for (; i < v->filters_len; i++)
    {
      char *name = (char *)vcf_header->id[BCF_DT_ID][v->filters[i]].key;
      printf ("  :filter \"%s\" ;\n", name);
    }

  printf ("  :quality %4.2f ", v->quality);

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
  v->position = NULL;
  v->confidence_interval = NULL;
  v->quality = 0.0;
  v->reference = NULL;
  v->alternative = NULL;
  v->id = NULL;
  v->filter = NULL;
  v->type = NULL;
  v->type_len = 0;
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

  variant->reference = buffer->d.allele[0];
  variant->alternative = buffer->d.allele[1];
  variant->id = buffer->d.id;
  variant->quality = buffer->qual;
  variant->filters_len = buffer->d.n_flt;
  variant->filters = buffer->d.flt;

  return true;
}
