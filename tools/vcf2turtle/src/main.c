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
#include <unistd.h>

#include <htslib/vcf.h>
#include <pthread.h>

#include "RuntimeConfiguration.h"
#include "GenomePosition.h"
#include "Variant.h"
#include "Origin.h"
#include "Sample.h"
#include "VcfHeader.h"
#include "ui.h"

#ifdef __GNUC__
#define UNUSED __attribute__((__unused__))
#else
#define UNUSED
#endif

extern RuntimeConfiguration program_config;
extern int hts_verbose;
static pthread_mutex_t output_mutex;

typedef struct
{
  bool is_reversed;
  bool is_left_of_ref;
  char *chromosome;
  int32_t position;
} BndProperties;

bool
parse_properties (BndProperties *properties,
                  const char *ref, int32_t ref_len,
                  const char *alt, int32_t alt_len)
{
  if (properties == NULL || ref == NULL || alt == NULL) return false;

  char bracket;

  /* Determine direction and inner-position.
   * ------------------------------------------------------------------------ */
  if (alt[0] == ']')
    {
      properties->is_reversed = false;
      properties->is_left_of_ref = true;
      bracket = ']';
    }
  else if (alt[0] == '[')
    {
      properties->is_reversed = true;
      properties->is_left_of_ref = true;
      bracket = '[';
    }
  else if (alt[alt_len - 1] == '[')
    {
      properties->is_reversed = false;
      properties->is_left_of_ref = false;
      bracket = '[';
    }
  else if (alt[alt_len - 1] == ']')
    {
      properties->is_reversed = true;
      properties->is_left_of_ref = false;
      bracket = ']';
    }

  /* Determine second chromosome and position.
   * ------------------------------------------------------------------------ */
  char *first_bracket = strchr (alt, bracket);
  char *last_bracket = strchr (first_bracket + 1, bracket);
  char *separator = strchr (alt, ':');

  if (first_bracket == NULL || last_bracket == NULL || separator == NULL)
    return false;

  properties->chromosome_len = separator - first_bracket - 1;
  int32_t position_len = last_bracket - separator - 1;

  properties->chromosome = calloc (sizeof (char), position->chromosome_len+1);

  if (properties->chromosome == NULL)
    return false;

  char pos[sizeof (char) * position_len + 1];
  memset (pos, 0, position_len + 1);

  memcpy (properties->chromosome, first_bracket+1, properties->chromosome_len);
  memcpy (pos, separator + 1, position_len);

  properties->position = atoi (pos);
  return properties;
}

/*----------------------------------------------------------------------------.
 | HANDLERS FOR SPECIFIC VCF RECORD TYPES                                     |
 '----------------------------------------------------------------------------*/

void
handle_record (Origin *origin, bcf_hdr_t *header, bcf1_t *buffer)
{
  variant_type = bcf_get_variant_types (buffer);

  /* Handle the program options for leaving out FILTER fields.
   * ------------------------------------------------------------------------ */
  if (program_config.filter &&
      bcf_has_filter (vcf_header, buffer, program_config.filter) == 1)
    {
      printf ("# Skipping %s record,\n", program_config.filter);
      return;
    }

  if (program_config.keep &&
      bcf_has_filter (vcf_header, buffer, program_config.keep) != 1)
    {
      printf ("# Skipping record without %s.\n", program_config.keep);
      return;
    }

  /* Unpack up and including the ALT field. */
  bcf_unpack (buffer, BCF_UN_STR);

  char *ref = buffer->d.allele[0];
  int32_t ref_len = buffer->rlen;

  /* TODO: Only the first alternative allele is considered.  When multiple
   * alleles are specified, we need to provide variants for each allele. */
  char *alt = buffer->d.allele[1];
  int32_t alt_len = strlen (alt);

  char *svtype = bcf_get_info (vcf_header, buffer, "SVTYPE");

  /* Handle the first genome position.
   * ------------------------------------------------------------------------ */
  bcf_get_info_int32 (vcf_header, buffer, "CIPOS", &(p1->cipos), &(p1->cipos_len));

  FaldoExactPosition start_position;
  faldo_init_position ((FaldoBaseType *)&start_position, FALDO_EXACT_POSITION);
  start_position->position = buffer->pos;
  start_position->chromosome = (char *)bcf_seqname (vcf_header, buffer);
  start_position->chromosome_len = strlen (chromosome);

  /* Complex rearrangements
   * ------------------------------------------------------------------------ */
  if (svtype != NULL && !strcmp (svtype "BND"))
    {
      BndProperties properties;
      bnd_and_properties_init (&properties);

      if (!parse_properties (&properties, ref, alt))
        puts ("# WARNING: Failed to parse complex rearrangement.");
      else
        {
          pthread_mutex_lock (&output_mutex);

          printf ("position:%s :hasDirection %s ; :atBreakPointPosition %s.\n",
                  faldo_exact_position_name (start_position)
                  (properties.is_reversed) ? ":ReverseComplement" : ":Same"
                  (properties.is_left_of_ref) ? ":Left" : ":Right");

          pthread_mutex_unlock (&output_mutex);
        }
    }

  /* Deletion events
   * ------------------------------------------------------------------------ */
  else if (ref_len > alt_len)
    {

    }

  /* Insertion/duplication events
   * ------------------------------------------------------------------------ */
  else if (ref_len < alt_len)
    {

    }

  pthread_mutex_lock (&output_mutex);
  faldo_exact_position_print (start_position);
  pthread_mutex_unlock (&output_mutex);
}

typedef struct
{
  htsFile *vcf_stream;
  bcf_hdr_t *vcf_header;
  bcf1_t **buffers;
  Origin *origin;
  int32_t thread_number;
  int32_t jobs_to_run;
} htslib_data_t;

void *
run_jobs_in_thread (void *data)
{
  htslib_data_t *pack = (htslib_data_t *)data;
  if (pack == NULL) return NULL;

  bcf_hdr_t *vcf_header = pack->vcf_header;
  Origin *origin = pack->origin;

  /* Process the records. */
  int32_t i = 0;
  int variant_type = 0;

  for (; i < pack->jobs_to_run; i++)
    {
      bcf1_t *buffer = pack->buffers[i + (pack->thread_number * pack->jobs_to_run)];
      handle_record (origin, vcf_header, buffer);
    }

  return NULL;
}


/*----------------------------------------------------------------------------.
 | HANDLE THE HEADER FIELDS                                                   |
 '----------------------------------------------------------------------------*/

void
handle_header (bcf_hdr_t *vcf_header, Origin *origin)
{
  if (vcf_header == NULL)
    return;

  int32_t i;
  for (i = 0; i < vcf_header->nhrec; i++)
    {
      /* Handle simple key-value fields.
       * ------------------------------------------------------------------- */
      if (vcf_header->hrec[i]->value)
        {
          VcfHeader h;
          initialize_VcfHeader (&h, HEADER_TYPE_GENERIC);
          h.origin = origin;
          h.key = vcf_header->hrec[i]->key;
          h.key_len = strlen (vcf_header->hrec[i]->key);
          h.value = vcf_header->hrec[i]->value;
          h.value_len = strlen (vcf_header->hrec[i]->value);
          h.origin = origin;

          print_VcfHeader (&h);
        }

      /* Handle INFO fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[i]->key, "INFO"))
        {
          int32_t j;
          VcfInfoField info_field;
          initialize_VcfHeader ((VcfHeader *)&info_field, HEADER_TYPE_INFO);
          info_field.origin = origin;

          for (j = 0; j < vcf_header->hrec[i]->nkeys; j++)
            {
              if (!strcmp (vcf_header->hrec[i]->keys[j], "ID"))
                info_field.id = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Number"))
                info_field.number = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Type"))
                info_field.type = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Description"))
                info_field.description = vcf_header->hrec[i]->vals[j];
            }

          print_VcfHeader ((VcfHeader *)&info_field);
        }

      /* Handle FILTER fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[i]->key, "FILTER"))
        {
          int32_t j;
          VcfFilterField filter_field;
          initialize_VcfHeader ((VcfHeader *)&filter_field, HEADER_TYPE_FILTER);
          filter_field.origin = origin;

          for (j = 0; j < vcf_header->hrec[i]->nkeys; j++)
            {
              if (!strcmp (vcf_header->hrec[i]->keys[j], "ID"))
                filter_field.id = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Description"))
                filter_field.description = vcf_header->hrec[i]->vals[j];
            }

          print_VcfHeader ((VcfHeader *)&filter_field);
        }

      /* Handle ALT fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[i]->key, "ALT"))
        {
          int32_t j;
          VcfAltField alt_field;
          initialize_VcfHeader ((VcfHeader *)&alt_field, HEADER_TYPE_ALT);
          alt_field.origin = origin;

          for (j = 0; j < vcf_header->hrec[i]->nkeys; j++)
            {
              if (!strcmp (vcf_header->hrec[i]->keys[j], "ID"))
                alt_field.id = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Description"))
                alt_field.description = vcf_header->hrec[i]->vals[j];
            }

          print_VcfHeader ((VcfHeader *)&alt_field);
        }

      /* Handle FORMAT fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[i]->key, "FORMAT"))
        {
          int32_t j;
          VcfFormatField format_field;
          initialize_VcfHeader ((VcfHeader *)&format_field, HEADER_TYPE_FORMAT);
          format_field.origin = origin;

          for (j = 0; j < vcf_header->hrec[i]->nkeys; j++)
            {
              if (!strcmp (vcf_header->hrec[i]->keys[j], "ID"))
                format_field.id = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Number"))
                format_field.number = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Type"))
                format_field.type = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "Description"))
                format_field.description = vcf_header->hrec[i]->vals[j];
            }

          print_VcfHeader ((VcfHeader *)&format_field);
        }

      /* Handle 'contig' fields.
       * ------------------------------------------------------------------- */
      else if (!strcmp (vcf_header->hrec[i]->key, "contig"))
        {
          int32_t j;
          VcfContigField contig_field;
          initialize_VcfHeader ((VcfHeader *)&contig_field, HEADER_TYPE_CONTIG);
          contig_field.origin = origin;

          for (j = 0; j < vcf_header->hrec[i]->nkeys; j++)
            {
              if (!strcmp (vcf_header->hrec[i]->keys[j], "ID"))
                contig_field.id = vcf_header->hrec[i]->vals[j];
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "length"))
                contig_field.length = atoi (vcf_header->hrec[i]->vals[j]);
              else if (!strcmp (vcf_header->hrec[i]->keys[j], "assembly"))
                contig_field.assembly = vcf_header->hrec[i]->vals[j];
            }

          print_VcfHeader ((VcfHeader *)&contig_field);
        }
      else
        fprintf (stderr, "Error: Encountered an unknown header item '%s'.\n",
                 vcf_header->hrec[i]->key);
    }
}

/*----------------------------------------------------------------------------.
 | MAIN PROGRAM                                                               |
 '----------------------------------------------------------------------------*/

int
main (int argc, char **argv)
{
  program_config.filter = NULL;
  program_config.keep = NULL;
  program_config.input_file = NULL;
  program_config.reference = "unknown";
  program_config.threads = 2;
  program_config.jobs_per_thread = 500;

  pthread_mutex_init(&output_mutex, NULL);

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
          { "filter",            required_argument, 0, 'f' },
          { "keep",              required_argument, 0, 'k' },
          { "graph-location",    required_argument, 0, 'g' },
          { "threads",           required_argument, 0, 't' },
          { "reference-genome",  required_argument, 0, 'r' },
          { "caller",            required_argument, 0, 'c' },
          { "threads",           required_argument, 0, 't' },
	  { "help",              no_argument,       0, 'h' },
	  { "version",           no_argument,       0, 'v' },
	  { 0,                   0,                 0, 0   }
	};

      while ( arg != -1 )
	{
	  /* Make sure to list all short options in the string below. */
	  arg = getopt_long (argc, argv, "i:f:g:k:r:c:t:vh", options, &index);
          switch (arg)
            {
            case 'i': program_config.input_file = optarg;            break;
            case 'f': program_config.filter = optarg;                break;
            case 'k': program_config.keep = optarg;                  break;
            case 'r': program_config.reference = optarg;             break;
            case 'c': program_config.caller = optarg;                break;
            case 't': program_config.threads = atoi(optarg);         break;
            case 'h': show_help ();                                  break;
            case 'v': show_version ();                               break;
            }
        }
    }
  else
    show_help ();

  /*--------------------------------------------------------------------------.
   | HANDLE AN INPUT FILE                                                     |
   '--------------------------------------------------------------------------*/

  if (program_config.input_file)
    {
      /* Show warnings for missing options that lead to missing data.
       * -------------------------------------------------------------------- */
      if (program_config.reference == NULL)
        fputs ("Warning: No --reference has been specified.  "
               "This may lead to incomplete and/or ambiguous information "
               "in the database.\n", stderr);

      if (program_config.caller == NULL)
        fputs ("Warning: No --caller has been specified.  "
               "This may lead to incomplete and/or ambiguous information "
               "in the database.\n", stderr);

      /* Disable the output messages from HTSLib.
       * -------------------------------------------------------------------- */
      hts_verbose = 0;

      /* Determine whether the input file is a VCF or compressed VCF.
       * -------------------------------------------------------------------- */
      int32_t input_file_len = strlen (program_config.input_file);
      bool is_vcf = false;
      bool is_gzip_vcf = false;
      bool is_bcf = false;
      bool is_gzip_bcf = false;

      if (input_file_len > 3)
        is_vcf = !strcmp (program_config.input_file + input_file_len - 3, "vcf");

      if (input_file_len > 3)
        is_bcf = !strcmp (program_config.input_file + input_file_len - 3, "bcf");

      if (!is_vcf && !is_bcf && input_file_len > 6)
        is_gzip_vcf = !strcmp (program_config.input_file + input_file_len - 6, "vcf.gz");

      if (!is_vcf && !is_bcf && input_file_len > 6)
        is_gzip_bcf = !strcmp (program_config.input_file + input_file_len - 6, "bcf.gz");

      if (!(is_bcf || is_gzip_bcf || is_vcf || is_gzip_vcf))
        return print_file_format_error ();

      /* Prepare the buffers needed to read the VCF file.
       * -------------------------------------------------------------------- */
      bcf_hdr_t *vcf_header = NULL;
      htsFile *vcf_stream = NULL;

      if (is_vcf)
        vcf_stream = hts_open (program_config.input_file, "r");
      else if (is_gzip_vcf)
        vcf_stream = hts_open (program_config.input_file, "rz");
      else if (is_bcf)
        vcf_stream = hts_open (program_config.input_file, "rbu");
      else if (is_gzip_bcf)
        vcf_stream = hts_open (program_config.input_file, "rb");

      if (vcf_stream == NULL)
        return print_vcf_file_error (program_config.input_file);

      vcf_header = bcf_hdr_read (vcf_stream);
      if (vcf_header == NULL)
        {
          hts_close (vcf_stream);
          return print_vcf_header_error (program_config.input_file);
        }

      /* Write the prefix of the Turtle output.
       * -------------------------------------------------------------------- */
      puts ("@prefix vcf:     <http://semweb.op.umcutrecht.nl/vcf/> .\n"
            "@prefix smo:     <http://semweb.op.umcutrecht.nl/smo/> .\n"
            "@prefix faldo:   <http://biohackathon.org/resource/faldo#> .\n"
            "@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
            "@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .");

      /* Add origin to database.
       * -------------------------------------------------------------------- */
      Origin o;
      initialize_Origin (&o);

      if (program_config.input_file[0] == '/')
        set_Origin_filename (&o, program_config.input_file);
      else
        {
          char *cwd = getcwd (NULL, 0);

          /* Degrade to a relative filename if we cannot allocate the memory
           * for an absolute path, because it's better than nothing. */
          if (cwd == NULL)
            set_Origin_filename (&o, program_config.input_file);
          else
            {
              int32_t full_path_len = strlen (cwd) + strlen (program_config.input_file) + 1;
              char full_path[full_path_len + 1];
              memset (full_path, 0, full_path_len);

              if (snprintf (full_path, full_path_len, "%s/%s",
                            cwd, program_config.input_file) == full_path_len)
                set_Origin_filename (&o, full_path);
              else
                /* Degrade to a relative filename if we cannot allocate the
                 * memory for an absolute path, because it's better than
                 * nothing. */
                set_Origin_filename (&o, program_config.input_file);

              free (cwd);
              cwd = NULL;
            }
        }

      print_Origin (&o);
      free (o.filename);
      o.filename = NULL;
      o.filename_len = 0;

      /* Add header lines to database.
       * -------------------------------------------------------------------- */
      handle_header (vcf_header, &o);
      
      /* Set up threads and per-thread storage.
       * -------------------------------------------------------------------- */
      pthread_t threads[program_config.threads];
      memset (threads, 0, sizeof (pthread_t) * program_config.threads);

      int32_t tbuffers_len = program_config.threads * program_config.jobs_per_thread;
      bcf1_t *tbuffers[tbuffers_len];

      int32_t j = 0;
      for (; j < tbuffers_len; j++)
        {
          tbuffers[j] = bcf_init ();
          if (tbuffers[j] == NULL)
            return print_memory_error (program_config.input_file);
        }

      /* Divide the work over the threads.
       * -------------------------------------------------------------------- */
      int32_t queue = 0;
      while (bcf_read (vcf_stream, vcf_header, tbuffers[queue]) == 0)
        {
          if (queue >= (program_config.jobs_per_thread * program_config.threads - 2))
            {
              htslib_data_t pack[j];
              /* Spawn the threads. */
              for (j = 0; j < program_config.threads; j++)
                {
                  pack[j].vcf_header = vcf_header;
                  pack[j].vcf_stream = vcf_stream;
                  pack[j].buffers = tbuffers;
                  pack[j].thread_number = j;
                  pack[j].jobs_to_run = program_config.jobs_per_thread;
                  pack[j].origin = &o;

                  pthread_create (&(threads[j]), NULL, run_jobs_in_thread, (void *)&(pack[j]));
                }

              /* Wait for all to complete. */
              for (j = 0; j < program_config.threads; j++)
                pthread_join (threads[j], NULL);

              /* Reset the queue */
              queue = 0;
            }
          else
            queue++;
        }

      if (queue > 0)
        {
          for (j = 0; j < queue; j++)
            {
              htslib_data_t pack;
              pack.vcf_header = vcf_header;
              pack.vcf_stream = vcf_stream;
              pack.buffers = tbuffers;
              pack.thread_number = 0;
              pack.jobs_to_run = 1;
              pack.origin = &o;

              /* Run them in a single thread. */
              run_jobs_in_thread (&pack);
            }
        }

      for (j = 0; j < tbuffers_len; j++)
        bcf_destroy (tbuffers[j]);

      bcf_hdr_destroy (vcf_header);
      hts_close (vcf_stream);
    }

  return 0;
}
