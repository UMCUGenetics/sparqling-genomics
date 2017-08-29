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
#include <math.h>
#include <gcrypt.h>

extern RuntimeConfiguration program_config;

char *
hash_Variant (Variant *v, bcf_hdr_t *vcf_header, bool use_cache)
{
  if (v == NULL) return NULL;
  if (v->position1 == NULL) return NULL;
  if (v->_obj_type == STRUCTURAL_VARIANT &&
      ((StructuralVariant *)v)->position2 == NULL) return NULL;

  /* Cache the hash generation. */
  if (v->hash[0] != '\0' && use_cache) return v->hash;

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

  /* Provide input for the hash. */
  gcry_md_write (handler,
                 hash_GenomePosition (v->position1, true),
                 HASH_LENGTH);

  /* Only StructuralVariant has a second position. */
  if (v->_obj_type == STRUCTURAL_VARIANT)
    gcry_md_write (handler,
                   hash_GenomePosition (((StructuralVariant *)v)->position2, true),
                   HASH_LENGTH);

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

  binary_hash = gcry_md_read (handler, 0);
  if (!get_pretty_hash (binary_hash, HASH_LENGTH, v->hash))
    {
      fprintf (stderr, "ERROR: Couldn't print a hash.\n");
      return NULL;
    }

  gcry_md_close (handler);
  return v->hash;
}

void
print_Variant (Variant *v, bcf_hdr_t *vcf_header)
{
  if (v == NULL) return;

  if (v->_obj_type == STRUCTURAL_VARIANT)
    printf ("v:%s a :StructuralVariant ;\n",
            hash_Variant (v, vcf_header, true));
  else if (v->_obj_type == SNP_VARIANT)
    printf ("v:%s a :SNPVariant ;\n",
            hash_Variant (v, vcf_header, true));
  else
    printf ("v:%s a :Variant ;\n",
            hash_Variant (v, vcf_header, true));

  if (v->origin)
    printf ("  :origin s:%s ;\n", hash_Origin (v->origin, true));

  printf ("  :genome_position p:%s ;\n",
          hash_GenomePosition (v->position1, true));

  if (v->_obj_type == STRUCTURAL_VARIANT)
    printf ("  :genome_position2 p:%s ;\n",
            hash_GenomePosition (((StructuralVariant *)v)->position2, true));

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
initialize_Variant (Variant *v)
{
  if (v == NULL) return;
  v->origin = NULL;
  v->position1 = NULL;
  v->quality = 0.0;
  v->filter = NULL;
  v->type = NULL;
  v->type_len = 0;
  memset (v->hash, '\0', 65);
  v->filters_len = 0;
  v->filters = NULL;
}

void
initialize_StructuralVariant (StructuralVariant *v)
{
  if (v == NULL) return;

  initialize_Variant ((Variant *)v);
  v->position2 = NULL;
}

void
reset_Variant (Variant *v)
{
  if (v == NULL) return;
  initialize_Variant (v);
}

void
reset_StructuralVariant (StructuralVariant *v)
{
  if (v == NULL) return;
  initialize_StructuralVariant (v);
}
