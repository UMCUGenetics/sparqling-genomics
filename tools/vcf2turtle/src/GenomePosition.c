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

#include "GenomePosition.h"
#include "RuntimeConfiguration.h"
#include "helper.h"

#include <stdio.h>
#include <gcrypt.h>

extern RuntimeConfiguration program_config;

char *
hash_GenomePosition (GenomePosition *g, bool use_cache)
{
  if (g == NULL) return NULL;

  /* Cache the hash generation. */
  if (g->hash[0] != '\0' && use_cache) return g->hash;

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

  unsigned char *binary_hash = NULL;

  int32_t position_strlen = 0;
  char position_str[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  position_strlen = sprintf (position_str, "%d", g->position);

  gcry_md_write (handler, g->chromosome, g->chromosome_len);
  gcry_md_write (handler, position_str, position_strlen);

  if (g->cipos_len > 0)
    {
      int32_t cipos_str_len = 0;
      char cipos_str[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

      cipos_str_len = sprintf (cipos_str, "%d%d", g->cipos[0], g->cipos[1]);
      gcry_md_write (handler, cipos_str, cipos_str_len);
    }

  binary_hash = gcry_md_read (handler, 0);
  if (!get_pretty_hash (binary_hash, HASH_LENGTH, g->hash))
    {
      fprintf (stderr, "ERROR: Couldn't print a hash.\n");
      return NULL;
    }

  gcry_md_close (handler);
  return g->hash;
}

void
print_GenomePosition (GenomePosition *g)
{
  if (g == NULL) return;

  printf ("p:%s a :GenomePosition ;\n", hash_GenomePosition (g, true));
  printf ("  :position %d ;\n", g->position);
  printf ("  :reference \"%s\" ;\n", g->reference);

  if (g->cipos_len > 0)
    {
      printf ("  :confidence_interval_start %d ;\n", g->cipos[0]);
      printf ("  :confidence_interval_end %d ;\n", g->cipos[1]);
    }

  printf ("  :chromosome \"%s\" .\n\n", g->chromosome);

  if (program_config.use_faldo)
    {
      printf ("p:%s a faldo:ExactPosition ;\n", hash_GenomePosition (g, true));
      printf ("  faldo:position %d ;\n", g->position);

      /* FIXME: We need a better way to describe the reference genome/contig. */
      printf ("  faldo:reference \"%s\" .\n\n", g->reference);
    }
}

void
initialize_GenomePosition (GenomePosition *g)
{
  if (g == NULL) return;

  g->chromosome = NULL;
  g->chromosome_len = 0;
  g->position = 0;
  memset (g->hash, '\0', 65);
  g->cipos_len = 0;
  g->cipos = NULL;
  g->reference = NULL;
}

void
reset_GenomePosition (GenomePosition *g)
{
  if (g == NULL) return;
  if (g->cipos) free (g->cipos);

  initialize_GenomePosition (g);
}
