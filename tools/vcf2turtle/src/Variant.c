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
#include "helper.h"
#include "RuntimeConfiguration.h"

#include <stdio.h>
#include <math.h>
#include <gcrypt.h>

extern RuntimeConfiguration program_config;

char *
hash_Variant (Variant *v, bcf_hdr_t *vcf_header, bool use_cache)
{
  if (v == NULL) return NULL;
  if (v->position1 == NULL || v->position2 == NULL) return NULL;

  /* Cache the hash generation. */
  if (v->hash != NULL && use_cache) return v->hash;

  /* FIXME: A quality of 0 is not the same as infinite.  So, find the
   * proper way to describe an infinite quality score. */
  if (!isfinite (v->quality))
    v->quality = 0;

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

  /* Provide input for the hash. */
  gcry_md_write (handler,
                 hash_GenomePosition (v->position1, true),
                 HASH_LENGTH);

  gcry_md_write (handler,
                 hash_GenomePosition (v->position2, true),
                 HASH_LENGTH);

  gcry_md_write (handler, quality_str, quality_strlen);

  if (v->type != NULL)
    gcry_md_write (handler, v->type, v->type_len);

  binary_hash = gcry_md_read (handler, 0);
  v->hash = get_pretty_hash (binary_hash, HASH_LENGTH);

  gcry_md_close (handler);
  return v->hash;
}

void
print_Variant (Variant *v, bcf_hdr_t *vcf_header)
{
  if (v == NULL) return;

  printf ("v:%s a :Variant ;\n", hash_Variant (v, vcf_header, true));
  printf ("  :genome_position  p:%s ;\n", hash_GenomePosition (v->position1, true));
  printf ("  :genome_position2 p:%s ;\n", hash_GenomePosition (v->position2, true));

  int i = 0;
  for (; i < v->filters_len; i++)
    {
      char *name = (char *)vcf_header->id[BCF_DT_ID][v->filters[i]].key;
      printf ("  :filter \"%s\" ;\n", name);
    }

  printf ("  :quality          %f ", v->quality);

  if (v->type)
    printf (";\n  :type          \"%s\" .\n\n", v->type);
  else
    printf (".\n\n");
}
