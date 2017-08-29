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

#include "Origin.h"
#include "RuntimeConfiguration.h"
#include "helper.h"

#include <stdio.h>
#include <gcrypt.h>

#define READ_BUFFER_MAX_LENGTH 4000000
extern RuntimeConfiguration program_config;

char *
hash_Origin (Origin *o, bool use_cache)
{
  if (o == NULL) return NULL;

  /* Cache the hash generation. */
  if (o->hash[0] != '\0' && use_cache) return o->hash;

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

  gcry_md_write (handler, o->filename, o->filename_len);

  binary_hash = gcry_md_read (handler, 0);
  if (!get_pretty_hash (binary_hash, HASH_LENGTH, o->hash))
    {
      fprintf (stderr, "ERROR: Couldn't print a hash.\n");
      return NULL;
    }

  gcry_md_close (handler);
  return o->hash;
}

void
print_Origin (Origin *o)
{
  if (o == NULL) return;

  printf ("o:%s a :Origin ;\n", hash_Origin (o, true));
  printf ("  :filename \"%s\" ;\n", o->filename);
  printf ("  :sha256_digest \"%s\" .\n\n", o->sha256_digest);
}

void
initialize_Origin (Origin *o)
{
  if (o == NULL) return;

  o->filename = NULL;
  memset (o->sha256_digest, 0, 65);
  memset (o->hash, 0, 65);
}

void
reset_Origin (Origin *o)
{
  if (o == NULL) return;
  if (o->filename) free (o->filename);

  initialize_Origin (o);
}

bool
set_Origin_filename (Origin *o, const char *filename)
{
  if (o == NULL) return false;

  o->filename = strdup (filename);
  if (o->filename == NULL) return false;

  o->filename_len = strlen (filename);

  /* Initialize GCrypt.
   * ------------------------------------------------------------------------ */
  gcry_error_t error;
  gcry_md_hd_t handler = NULL;
  unsigned char *binary_digest = NULL;

  error = gcry_md_open (&handler, GCRY_MD_SHA256, 0);
  if (error)
    {
      fprintf (stderr, "ERROR: %s/%s\n",
               gcry_strsource (error),
               gcry_strerror (error));
      return NULL;
    }

  
  /* Open the file.
   * ------------------------------------------------------------------------ */
  FILE *file = fopen (o->filename, "rb");
  char *buffer = NULL;
  long file_size;

  if (file == NULL)
    {
      fprintf (stderr, "ERROR: Could not open '%s'.\n", o->filename);
      return false;
    }

  fseek (file, 0L, SEEK_END);
  file_size = ftell (file);
  rewind (file);

  buffer = calloc (sizeof (char), READ_BUFFER_MAX_LENGTH);

  if (buffer == NULL)
    {
      fprintf (stderr, "ERROR: Cannot allocate enough memory.\n");
      return false;
    }

  /* Process the file.
   * ------------------------------------------------------------------------ */
  size_t bytes_read = 0;
  bytes_read = fread (buffer, sizeof (char), READ_BUFFER_MAX_LENGTH, file);
  while (bytes_read == READ_BUFFER_MAX_LENGTH)
    {
      gcry_md_write (handler, buffer, bytes_read);
      memset (buffer, 0, bytes_read);
      bytes_read = fread (buffer, sizeof (char), READ_BUFFER_MAX_LENGTH, file);
    }

  gcry_md_write (handler, buffer, bytes_read);
  memset (buffer, 0, READ_BUFFER_MAX_LENGTH * sizeof (char));
  bytes_read = 0;

  fclose (file);

  binary_digest = gcry_md_read (handler, 0);
  if (!get_pretty_hash (binary_digest, HASH_LENGTH, o->sha256_digest))
    {
      fprintf (stderr, "ERROR: Couldn't print a hash.\n");
      return false;
    }

  free (buffer);
  buffer = NULL;
  gcry_md_close (handler);

  return true;
}
