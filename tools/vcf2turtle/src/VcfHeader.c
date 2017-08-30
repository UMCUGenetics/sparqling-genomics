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

  if (v->_type == HEADER_TYPE_GENERIC)
    {
      gcry_md_write (handler, "GENERIC", 7);
      gcry_md_write (handler, v->key, v->key_len);
      gcry_md_write (handler, v->value, v->value_len);
    }
  else if (v->_type == HEADER_TYPE_FILTER)
    {
      VcfFilterField *f = (VcfFilterField *)v;
      gcry_md_write (handler, "FILTER", 6);
      gcry_md_write (handler, f->id, f->id_len);
      gcry_md_write (handler, f->description, f->description_len);
    }
  else if (v->_type == HEADER_TYPE_ALT)
    {
      VcfAltField *f = (VcfAltField *)v;
      gcry_md_write (handler, "ALT", 3);
      gcry_md_write (handler, f->id, f->id_len);
      gcry_md_write (handler, f->description, f->description_len);
    }
  else if (v->_type == HEADER_TYPE_INFO)
    {
      VcfInfoField *i = (VcfInfoField *)v;
      gcry_md_write (handler, "INFO", 4);
      gcry_md_write (handler, i->id, i->id_len);
      gcry_md_write (handler, i->type, i->type_len);
      gcry_md_write (handler, i->number, i->number_len);
      gcry_md_write (handler, i->description, i->description_len);
    }
  else if (v->_type == HEADER_TYPE_FORMAT)
    {
      VcfFormatField *i = (VcfFormatField *)v;
      gcry_md_write (handler, "FORMAT", 6);
      gcry_md_write (handler, i->id, i->id_len);
      gcry_md_write (handler, i->type, i->type_len);
      gcry_md_write (handler, i->number, i->number_len);
      gcry_md_write (handler, i->description, i->description_len);
    }
  else if (v->_type == HEADER_TYPE_CONTIG)
    {
      VcfContigField *i = (VcfContigField *)v;
      gcry_md_write (handler, "CONTIG", 6);
      gcry_md_write (handler, i->id, i->id_len);

      int32_t length_strlen = 0;
      char length_str[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
      length_strlen = sprintf (length_str, "%d", i->length);
      gcry_md_write (handler, length_str, length_strlen);

      gcry_md_write (handler, i->assembly, i->assembly_len);
    }

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

  if (v->_type == HEADER_TYPE_GENERIC)
    {
      printf ("h:%s a :VcfGenericHeader ;\n", hash_VcfHeader (v, true));
      printf ("  :type \"GENERIC\" ;\n");
      printf ("  :key \"%s\" ;\n", v->key);
      printf ("  :value \"%s\" ;\n", v->value);
    }
  else if (v->_type == HEADER_TYPE_FILTER)
    {
      VcfFilterField *f = (VcfFilterField *)v;
      printf ("h:%s a :VcfInfoHeader ;\n", hash_VcfHeader (v, true));
      printf ("  :type \"FILTER\" ;\n");
      printf ("  :id \"%s\" ;\n", f->id);
      printf ("  :description %s ;\n", f->description);
    }
  else if (v->_type == HEADER_TYPE_ALT)
    {
      VcfAltField *f = (VcfAltField *)v;
      printf ("h:%s a :VcfAltHeader ;\n", hash_VcfHeader (v, true));
      printf ("  :type \"ALT\" ;\n");
      printf ("  :id \"%s\" ;\n", f->id);
      printf ("  :description %s ;\n", f->description);
    }
  else if (v->_type == HEADER_TYPE_INFO)
    {
      VcfInfoField *i = (VcfInfoField *)v;
      printf ("h:%s a :VcfInfoHeader ;\n", hash_VcfHeader (v, true));
      printf ("  :type \"INFO\" ;\n");
      printf ("  :id \"%s\" ;\n", i->id);
      printf ("  :number \"%s\" ;\n", i->number);
      printf ("  :description %s ;\n", i->description);
    }
  else if (v->_type == HEADER_TYPE_FORMAT)
    {
      VcfFormatField *i = (VcfFormatField *)v;
      printf ("h:%s a :VcfFormatHeader ;\n", hash_VcfHeader (v, true));
      printf ("  :type \"FORMAT\" ;\n");
      printf ("  :id \"%s\" ;\n", i->id);
      printf ("  :number \"%s\" ;\n", i->number);
      printf ("  :description %s ;\n", i->description);
    }
  else if (v->_type == HEADER_TYPE_CONTIG)
    {
      VcfContigField *i = (VcfContigField *)v;
      printf ("h:%s a :VcfContigHeader ;\n", hash_VcfHeader (v, true));
      printf ("  :type \"contig\" ;\n");
      printf ("  :id \"%s\" ;\n", i->id);
      printf ("  :length %d ;\n", i->length);
      printf ("  :assembly %s ;\n", i->assembly);
    }

  printf ("  :origin o:%s .\n\n", hash_Origin (v->origin, true));
}

void
initialize_VcfHeader (VcfHeader *v, HeaderType type)
{
  if (v == NULL) return;
  if (type == HEADER_TYPE_GENERIC)
    memset (v, 0, sizeof (*v));
  else if (type == HEADER_TYPE_FILTER)
    memset (v, 0, sizeof (VcfFilterField));
  else if (type == HEADER_TYPE_ALT)
    memset (v, 0, sizeof (VcfAltField));
  else if (type == HEADER_TYPE_INFO)
    memset (v, 0, sizeof (VcfInfoField));
  else if (type == HEADER_TYPE_FORMAT)
    memset (v, 0, sizeof (VcfFormatField));
  else if (type == HEADER_TYPE_CONTIG)
    memset (v, 0, sizeof (VcfContigField));

  v->_type = type;
  v->origin = NULL;
  memset (v->hash, 0, 65);
}

void
reset_VcfHeader (VcfHeader *v)
{
  if (v == NULL) return;
  if (v->key) free (v->key);
  if (v->value) free (v->value);
  if (v->origin) v->origin = NULL;

  initialize_VcfHeader (v, v->_type);
}
