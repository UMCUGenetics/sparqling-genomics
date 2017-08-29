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

#include "VcfHeader.h"
#include "RuntimeConfiguration.h"
#include "helper.h"

#include <stdio.h>
#include <gcrypt.h>

extern RuntimeConfiguration program_config;

char *
hash_VcfHeader (VcfHeader *v, bool use_cache)
{
  if (v == NULL) return NULL;

  /* Cache the hash generation. */
  if (v->hash[0] != '\0' && use_cache) return v->hash;

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

  gcry_md_write (handler, v->type, v->type_len);
  gcry_md_write (handler, v->key, v->key_len);
  gcry_md_write (handler, v->value, v->value_len);
  gcry_md_write (handler, hash_Origin (v->origin, true), 64);

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
print_VcfHeader (VcfHeader *v)
{
  if (v == NULL) return;

  printf ("h:%s a :VcfHeader ;\n", hash_VcfHeader (v, true));
  printf ("  :type \"%s\" ;\n", v->type);
  printf ("  :key \"%s\" ;\n", v->key);
  printf ("  :value \"%s\" ;\n", v->value);
  printf ("  :origin o:%s .\n\n", hash_Origin (v->origin, true));
}

void
initialize_VcfHeader (VcfHeader *v)
{
  if (v == NULL) return;

  v->type = NULL;
  v->type_len = 0;
  v->key = NULL;
  v->value = NULL;
  v->value_len = 0;
  v->origin = NULL;
  memset (v->hash, 0, 65);
}

void
reset_VcfHeader (VcfHeader *v)
{
  if (v == NULL) return;
  if (v->type) free (v->type);
  if (v->key) free (v->key);
  if (v->value) free (v->value);
  if (v->origin) v->origin = NULL;

  initialize_VcfHeader (v);
}
