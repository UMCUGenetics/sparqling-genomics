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

#include <htslib/vcf.h>

#include "RuntimeConfiguration.h"
#include "GenomePosition.h"
#include "Variant.h"

extern RuntimeConfiguration program_config;
const char DEFAULT_GRAPH_LOCATION[] = "http://localhost:8890/TestGraph/";

/*----------------------------------------------------------------------------.
 | USER INTERFACE HELPERS                                                     |
 '----------------------------------------------------------------------------*/

static void
show_help (void)
{
  puts ("\nAvailable options:\n"
        "  --input-file,        -i  The input to process.\n"
        "  --filter-lowqual,    -f  Do not process calls with FILTER=LowQual.\n"
        "  --only-keep-lowqual, -o  Do not process calls without FILTER=LowQual.\n"
        "                           This option cannot be used together with -f.\n"
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
  if (program_config.filter_lowqual_calls &&
      bcf_has_filter (vcf_header, buffer, "LowQual") == 1)
    {
      puts ("# Skipping record because of LowQual filter");
      return;
    }

  if (program_config.only_keep_lowqual_calls &&
      bcf_has_filter (vcf_header, buffer, "LowQual") != 1)
    {
      puts ("# Skipping record without LowQual filter.");
      return;
    }

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

      /* /\* Make sure the buffer->d is filled, so we can access the filters. *\/ */
      /* bcf_unpack (buffer, BCF_UN_FLT); */

      /* int32_t i = 0; */
      /* for (; i < buffer->d.n_flt; i++) */
      /*   { */
      /*     /\* Now this is a chain of dereferencing..  The program might crash */
      /*      * here if we don't carefully check whether everything is alright. *\/ */
      /*     bcf_hrec_t *hrec = ((bcf_idinfo_t *)vcf_header->dict[BCF_DT_ID])->hrec[buffer->d.flt[i]]; */

      /*     printf ("# Filter: key: '%s'\n", hrec->key); */
      /*     printf ("# Filter: value: '%s'\n", hrec->vals); */
      /*   } */

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
  program_config.filter_lowqual_calls = false;
  program_config.only_keep_lowqual_calls = false;
  program_config.input_file = NULL;
  program_config.graph_location = (char *)DEFAULT_GRAPH_LOCATION;

  /*--------------------------------------------------------------------------.
   | PROCESS COMMAND-LINE OPTIONS                                             |
   '--------------------------------------------------------------------------*/

  if (argc > 1)
    {
      int arg = 0;
      int index = 0;

      /* Program options
       * ------------------------------------------------------------------- */
      static struct option options[] =
	{
          { "input-file",        required_argument, 0, 'i' },
          { "filter-lowqual",    no_argument,       0, 'f' },
          { "only-keep-lowqual", no_argument,       0, 'o' },
          { "graph-location",    required_argument, 0, 'g' },
	  { "help",              no_argument,       0, 'h' },
	  { "version",           no_argument,       0, 'v' },
	  { 0,                   0,                 0, 0   }
	};

      while ( arg != -1 )
	{
	  /* Make sure to list all short options in the string below. */
	  arg = getopt_long (argc, argv, "i:fg:ovh", options, &index);
          switch (arg)
            {
            case 'i': program_config.input_file = optarg;            break;
            case 'f': program_config.filter_lowqual_calls = true;    break;
            case 'o': program_config.only_keep_lowqual_calls = true; break;
            case 'g': program_config.graph_location = optarg;        break;
            case 'h': show_help ();                                  break;
            case 'v': show_version ();                               break;
            }
        }
    }
  else
    show_help ();

  /*--------------------------------------------------------------------------.
   | DON'T WASTE TIME WITH SHORT-CIRCUITS                                     |
   '--------------------------------------------------------------------------*/
  if (program_config.filter_lowqual_calls && program_config.only_keep_lowqual_calls)
    {
      puts ("# Short-circuit: You specified both -f and -o.");
      return 0;
    }

  /*--------------------------------------------------------------------------.
   | HANDLE AN INPUT FILE                                                     |
   '--------------------------------------------------------------------------*/

  if (program_config.input_file)
    {
      /* Determine whether the input file is a VCF or compressed VCF.
       * -------------------------------------------------------------------- */
      int32_t input_file_len = strlen (program_config.input_file);
      bool is_vcf = false;
      bool is_gzip_vcf = false;

      if (input_file_len > 3)
        is_vcf = !strcmp (program_config.input_file + input_file_len - 3, "vcf");

      if (!is_vcf && input_file_len > 6)
        is_gzip_vcf = !strcmp (program_config.input_file + input_file_len - 6, "vcf.gz");

      if (!(is_vcf || is_gzip_vcf))
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
        vcf_stream = hts_open (program_config.input_file, "r");
      else if (is_gzip_vcf)
        vcf_stream = hts_open (program_config.input_file, "rz");

      if (vcf_stream == NULL)
        return print_vcf_file_error (program_config.input_file);

      vcf_header = bcf_hdr_read (vcf_stream);
      if (vcf_header == NULL)
        {
          hts_close (vcf_stream);
          return print_vcf_header_error (program_config.input_file);
        }

      buffer = bcf_init ();
      if (buffer == NULL) return print_memory_error (program_config.input_file);

      /* Write the prefix of the Turtle output.
       * -------------------------------------------------------------------- */
      printf ("@prefix : <%s> .\n", program_config.graph_location);
      printf ("@prefix v: <%sVariant/> .\n", program_config.graph_location);
      printf ("@prefix p: <%sPosition/> .\n", program_config.graph_location);
      printf ("@prefix s: <%sSample/> .\n\n", program_config.graph_location);

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
