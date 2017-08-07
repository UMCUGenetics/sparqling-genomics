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

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <getopt.h>
#include <math.h>

#include <gcrypt.h>
#include <htslib/vcf.h>

/* Note: The HASH_LENGTH will be 32, but a pretty-printed hexadecimal version
 * of it will need 64 bytes of space. */
#define HASH_ALGORITHM GCRY_MD_SHA256
#define HASH_LENGTH    gcry_md_get_algo_dlen (HASH_ALGORITHM)


/*----------------------------------------------------------------------------.
 | GENERAL HELPER FUNCTIONS                                                   |
 '----------------------------------------------------------------------------*/

char *
get_pretty_hash (unsigned char *hash, uint32_t length)
{
  char *output = calloc (1, length * 2 + 1);
  if (output == NULL) return NULL;

  uint32_t i = 0;
  for (; i < (length); i++)
    if (snprintf (output + (i * 2), 3, "%02x", hash[i]) != 2)
      {
        free (output);
        return NULL;
      }

  return output;
}


/*----------------------------------------------------------------------------.
 | DATA TYPE DEFINITIONS                                                      |
 '----------------------------------------------------------------------------*/

typedef struct
{
  char *chromosome;
  uint32_t chromosome_len;
  uint32_t position;
  char *hash;
} GenomePosition;

char *
hash_GenomePosition (GenomePosition *g, bool use_cache)
{
  if (g == NULL) return NULL;

  /* Cache the hash generation. */
  if (g->hash != NULL && use_cache) return g->hash;

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

  binary_hash = gcry_md_read (handler, 0);
  g->hash = get_pretty_hash (binary_hash, HASH_LENGTH);

  gcry_md_close (handler);
  return g->hash;
}

void
print_GenomePosition (GenomePosition *g)
{
  if (g == NULL) return;

  printf ("p:%s a :GenomePosition ;\n", hash_GenomePosition (g, true));
  printf ("  :position   %d ;\n", g->position);
  printf ("  :chromosome \"%s\" .\n\n", g->chromosome);
}

typedef struct
{
  GenomePosition *position1;
  GenomePosition *position2;
  float quality;
  char *filter;
  char *type;
  uint32_t type_len;
  char *hash;
} Variant;

char *
hash_Variant (Variant *v, bool use_cache)
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
print_Variant (Variant *v)
{
  if (v == NULL) return;

  printf ("v:%s a :Variant ;\n", hash_Variant (v, true));
  printf ("  :genome_position  p:%s ;\n", hash_GenomePosition (v->position1, true));
  printf ("  :genome_position2 p:%s ;\n", hash_GenomePosition (v->position2, true));
  printf ("  :quality          %f ", v->quality);
  //printf ("  :filter          \"%s\" ", v->filter);

  if (v->type)
    printf (";\n  :type          \"%s\" .\n\n", v->type);
  else
    printf (".\n\n");
}


/*----------------------------------------------------------------------------.
 | USER INTERFACE HELPERS                                                     |
 '----------------------------------------------------------------------------*/

static void
show_help (void)
{
  puts ("\nAvailable options:\n"
        "  --input-file,        -i  The input to process.\n"
	"  --version,           -v  Show versioning information.\n"
	"  --help,              -h  Show this message.\n\n");
}

static void
show_version (void)
{
  puts ("Version: 0.0.1\n");
}

static int32_t
print_vcf_file_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot open '%s'.\n", file_name);
  return 1;
}

static int32_t
print_vcf_header_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot read header of '%s'.\n", file_name);
  return 1;
}

static int32_t
print_memory_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Not enough memory available for processing '%s'.\n", file_name);
  return 1;
}


/*----------------------------------------------------------------------------.
 | HANDLERS FOR SPECIFIC VCF RECORD TYPES                                     |
 '----------------------------------------------------------------------------*/

void
handle_REF_record (bcf_hdr_t *vcf_header, bcf1_t *buffer)
{
  puts ("# REF records have not been implemented (yet).");
}

void
handle_SNP_record (bcf_hdr_t *vcf_header, bcf1_t *buffer)
{
  puts ("# SNP records have not been implemented (yet).");
}

void
handle_MNP_record (bcf_hdr_t *vcf_header, bcf1_t *buffer)
{
  puts ("# MNP records have not been implemented (yet).");
}

void
handle_INDEL_record (bcf_hdr_t *vcf_header, bcf1_t *buffer)
{
  puts ("# INDEL records have not been implemented (yet).");
}

void
handle_OTHER_record (bcf_hdr_t *vcf_header, bcf1_t *buffer)
{

  /* Delly SV output seems to fall into this category. */
  bcf_info_t *end_info = bcf_get_info (vcf_header, buffer, "END");
  bcf_info_t *chr2_info = bcf_get_info (vcf_header, buffer, "CHR2");

  if (end_info != NULL && chr2_info != NULL)
    {
      GenomePosition p1 = {
        .chromosome = NULL,
        .chromosome_len = 0,
        .position = 0,
        .hash = NULL
      };

      p1.chromosome = (char *)bcf_seqname (vcf_header, buffer);
      p1.chromosome_len = strlen (p1.chromosome);

      GenomePosition p2 = {
        .chromosome = NULL,
        .chromosome_len = 0,
        .position = 0,
        .hash = NULL
      };

      p2.chromosome = calloc (1, chr2_info->len + 1);
      p2.chromosome_len = chr2_info->len;

      if (p2.chromosome == NULL)
        {
          printf ("# ERROR allocating memory.\n");
          return;
        }

      memcpy (p2.chromosome, chr2_info->vptr, chr2_info->len);

      p1.position = buffer->pos;
      p2.position = end_info->v1.i;

      print_GenomePosition (&p1);
      print_GenomePosition (&p2);

      /* The hash has been generated, and the data has been printed.  So, we
       * no longer need to have this data allocated.  Warning:  If you ever
       * implement a feature that does not use the cache of GenomePositions,
       * then remove this code and handle the free()'ing of this string in
       * the appropriate place. */
      free (p2.chromosome);
      p2.chromosome_len = 0;

      Variant v = {
        .position1 = &p1,
        .position2 = &p2,
        .quality = buffer->qual,
        .filter = NULL,
        .type = NULL,
        .type_len = 0,
        .hash = NULL
      };

      print_Variant (&v);

      /* Free the memory for the hashes. */
      free (p1.hash);
      free (p2.hash);
      free (v.hash);
    }
  else
    {
      puts ("# OTHER records without END or CHR2 properties have "
            "not been implemented (yet).");
    }
}

void
handle_BND_record (bcf_hdr_t *vcf_header, bcf1_t *buffer)
{
  puts ("# BND records have not been implemented (yet).");
}

/*----------------------------------------------------------------------------.
 | MAIN PROGRAM                                                               |
 '----------------------------------------------------------------------------*/

int
main (int argc, char **argv)
{
  /*--------------------------------------------------------------------------.
   | PROCESS COMMAND-LINE OPTIONS                                             |
   '--------------------------------------------------------------------------*/

  char *input_file = NULL;
  if (argc > 1)
    {
      int arg = 0;
      int index = 0;

      /* Program options
       * ------------------------------------------------------------------- */
      static struct option options[] =
	{
          { "input-file",        required_argument, 0, 'i' },
	  { "help",              no_argument,       0, 'h' },
	  { "version",           no_argument,       0, 'v' },
	  { 0,                   0,                 0, 0   }
	};

      while ( arg != -1 )
	{
	  /* Make sure to list all short options in the string below. */
	  arg = getopt_long (argc, argv, "i:vh", options, &index);
          switch (arg)
            {
            case 'i': input_file = optarg; break;
            case 'h': show_help ();        break;
            case 'v': show_version ();     break;
            }
        }
    }
  else
    show_help ();

  /*--------------------------------------------------------------------------.
   | HANDLE AN INPUT FILE                                                     |
   '--------------------------------------------------------------------------*/

  if (input_file)
    {
      /* Determine whether the input file is a VCF or compressed VCF.
       * -------------------------------------------------------------------- */
      int32_t input_file_len = strlen (input_file);
      bool is_vcf = false;
      bool is_compressed_vcf = false;

      if (input_file_len > 3)
        is_vcf = !strcmp (input_file + input_file_len - 3, "vcf");

      if (!is_vcf && input_file_len > 6)
        is_compressed_vcf = !strcmp (input_file + input_file_len - 6, "vcf.gz");

      if (!(is_vcf || is_compressed_vcf))
        {
          fprintf (stderr, "ERROR: This program only handles \".vcf\" and \".vcf.gz\" files.");
          return 1;
        }

      /* Prepare the buffers needed to read the VCF file.
       * -------------------------------------------------------------------- */
      bcf1_t *buffer = NULL;
      bcf_hdr_t *vcf_header = NULL;
      htsFile *vcf_stream = NULL;

      if (is_vcf)
        vcf_stream = hts_open (input_file, "r");
      else if (is_compressed_vcf)
        vcf_stream = hts_open (input_file, "rz");

      if (vcf_stream == NULL)
        return print_vcf_file_error (input_file);

      vcf_header = bcf_hdr_read (vcf_stream);
      if (vcf_header == NULL)
        {
          hts_close (vcf_stream);
          return print_vcf_header_error (input_file);
        }

      buffer = bcf_init ();
      if (buffer == NULL) return print_memory_error (input_file);

      /* Write the prefix of the Turtle output.
       * -------------------------------------------------------------------- */
      puts ("@prefix : <http://localhost:8890/TestGraph/> .");
      puts ("@prefix v: <http://localhost:8890/TestGraph/Variant/> .");
      puts ("@prefix p: <http://localhost:8890/TestGraph/Position/> .");
      puts ("@prefix s: <http://localhost:8890/TestGraph/Sample/> .");
      puts ("");

      while (bcf_read (vcf_stream, vcf_header, buffer) == 0)
        {
          /* A VCF file can contain all kinds of data.  Each is represented
           * a little different in the graph model. */
          int variant_type = bcf_get_variant_types (buffer);
          switch (variant_type)
            {
            case VCF_REF:    handle_REF_record (vcf_header, buffer);    break;
            case VCF_SNP:    handle_SNP_record (vcf_header, buffer);    break;
            case VCF_MNP:    handle_MNP_record (vcf_header, buffer);    break;
            case VCF_INDEL:  handle_INDEL_record (vcf_header, buffer);  break;
            case VCF_OTHER:  handle_OTHER_record (vcf_header, buffer);  break;
            case VCF_BND:    handle_BND_record (vcf_header, buffer);    break;
            default:
              puts ("# Encountered an unknown variant call type.");
              break;
            }

          /* Avoid reallocation of the buffer, and instead clear the contents
           * of the variables inside the buffer. */
          bcf_clear (buffer);
        }

      bcf_destroy (buffer);
      bcf_hdr_destroy (vcf_header);
      hts_close (vcf_stream);
    }

  return 0;
}
