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

#include "Sample.h"
#include "RuntimeConfiguration.h"
#include "helper.h"

#include <stdio.h>
#include <string.h>
#include <gcrypt.h>

extern RuntimeConfiguration program_config;

char *
hash_Sample (Sample *s, bool use_cache)
{
  if (s == NULL) return NULL;

  /* Cache the hash generation. */
  if (s->hash[0] != '\0' && use_cache) return s->hash;

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

  gcry_md_write (handler, s->filename, s->filename_len);

  binary_hash = gcry_md_read (handler, 0);
  if (!get_pretty_hash (binary_hash, HASH_LENGTH, s->hash))
    {
      fprintf (stderr, "ERROR: Couldn't print a hash.\n");
      return NULL;
    }

  gcry_md_close (handler);
  return s->hash;
}

void
print_Sample (Sample *s)
{
  if (s == NULL) return;

  printf ("s:%s a :Sample ;\n", hash_Sample (s, true));
  printf ("  :filename \"%s\" .\n\n", s->filename);
}

void
initialize_Sample (Sample *s)
{
  if (s == NULL) return;
  s->filename = NULL;
  s->filename_len = 0;
  memset (s->hash, '\0', 65);
}

void
reset_Sample (Sample *s)
{
  if (s == NULL) return;

  free (s->filename);
  initialize_Sample (s);
}

bool
set_Sample_filename (Sample *s, const char *filename)
{
  if (s == NULL) return false;

  s->filename = strdup (filename);
  if (s->filename == NULL) return false;

  s->filename_len = strlen (filename);
  return true;
}
