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

#ifdef __GNUC__
#define UNUSED __attribute__((__unused__))
#else
#define UNUSED
#endif

extern RuntimeConfiguration program_config;
extern int hts_verbose;

const char DEFAULT_GRAPH_LOCATION[] = "http://localhost:8890/TestGraph/";
static pthread_mutex_t output_mutex;

/*----------------------------------------------------------------------------.
 | USER INTERFACE HELPERS                                                     |
 '----------------------------------------------------------------------------*/

static void
show_help (void)
{
  puts ("\nAvailable options:\n"
        "  --input-file=ARG,   -i  The input file to process.\n"
        "  --filter=ARG,       -f  Omit calls with FILTER=ARG from the "
                                   "output.\n"
        "  --keep=ARG,         -k  Omit calls without FILTER=ARG from the "
                                   "output.\n"
        "  --graph-location,   -g  Location of the graph.\n"
        "  --use-faldo,            Add triples from the Feature Annotation\n"
        "                          Location Description Ontology (FALDO).\n"
        "  --reference-genome  -r  The reference genome the variant positions "
                                  "refer to.\n"
	"  --threads,          -t  Number of threads to use.\n"
	"  --version,          -v  Show versioning information.\n"
	"  --help,             -h  Show this message.\n");
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

static int32_t
print_file_format_error ()
{
  fprintf (stderr, "ERROR: This program only handles \".vcf\" and \".vcf.gz\" files.\n");
  return 1;
}


/*----------------------------------------------------------------------------.
 | HANDLERS FOR SPECIFIC VCF RECORD TYPES                                     |
 '----------------------------------------------------------------------------*/

void
handle_REF_record (UNUSED bcf_hdr_t *vcf_header, UNUSED bcf1_t *buffer)
{
  puts ("# REF records have not been implemented (yet).");
}

void
handle_SNP_record (bcf_hdr_t *vcf_header, bcf1_t *buffer,
                   GenomePosition *position, Origin *origin)
{
  /* Do not process it when it isn't a SNP.
   * ------------------------------------------------------------------------ */
  bool is_snp = bcf_is_snp (buffer);
  if (!is_snp)
    {
      printf ("# 'handle_SNP_record' processed a non-SNP variant.");
      return;
    }

  /* Set the confidence interval.
   * ------------------------------------------------------------------------ */
  bcf_get_info_int32 (vcf_header, buffer, "CIPOS",
                      &(position->cipos),
                      &(position->cipos_len));

  /* Set the chromosome and position.
   * ------------------------------------------------------------------------ */
  position->chromosome = (char *)bcf_seqname (vcf_header, buffer);
  position->chromosome_len = strlen (position->chromosome);
  position->position = buffer->pos;
  position->reference = program_config.reference;

  /* Output the position.
   * ------------------------------------------------------------------------ */
  pthread_mutex_lock (&output_mutex);
  print_GenomePosition (position);
  pthread_mutex_unlock (&output_mutex);

  /* Handle the variant information.
   * ------------------------------------------------------------------------ */
  SNPVariant v;
  initialize_SNPVariant (&v);

  v.origin = origin;
  v._obj_type = SNP_VARIANT;
  v.position1 = position;

  /* Set the 'quality' field.
   * ------------------------------------------------------------------------ */
  v.quality = buffer->qual;

  /* Make sure the FILTER field is available.
   * ------------------------------------------------------------------------ */
  if (!(buffer->unpacked & BCF_UN_FLT))
    bcf_unpack(buffer, BCF_UN_FLT);

  v.filters_len = buffer->d.n_flt;
  v.filters = buffer->d.flt;

  /* Output the Variant.
   * ------------------------------------------------------------------------ */
  pthread_mutex_lock (&output_mutex);
  print_SNPVariant ((Variant *)&v, vcf_header);
  pthread_mutex_unlock (&output_mutex);

  /* Clean up the memory.
  * ------------------------------------------------------------------------- */
  reset_GenomePosition (position);
  reset_SNPVariant (&v);
}

void
handle_MNP_record (UNUSED bcf_hdr_t *vcf_header, UNUSED bcf1_t *buffer)
{
  puts ("# MNP records have not been implemented (yet).");
}

void
handle_INDEL_record (UNUSED bcf_hdr_t *vcf_header, UNUSED bcf1_t *buffer)
{
  puts ("# INDEL records have not been implemented (yet).");
}

void
handle_OTHER_record (bcf_hdr_t *vcf_header, bcf1_t *buffer,
                     GenomePosition *p1, GenomePosition *p2,
                     Origin *o)
{
  /* Handle the program options for leaving out FILTER fields.
   * ------------------------------------------------------------------------ */
  if (program_config.filter &&
      bcf_has_filter (vcf_header, buffer, program_config.filter) == 1)
    {
      printf ("# Skipping record because of %s filter,\n",
              program_config.filter);
      return;
    }

  if (program_config.keep &&
      bcf_has_filter (vcf_header, buffer, program_config.keep) != 1)
    {
      printf ("# Skipping record without %s filter.\n",
              program_config.keep);
      return;
    }

  /* Check for the SVTYPE property.
   * ------------------------------------------------------------------------ */
  bcf_info_t *svtype_info = bcf_get_info (vcf_header, buffer, "SVTYPE");
  bool is_sv = (svtype_info != NULL);

  /* Handle the first genome position.
   * ------------------------------------------------------------------------ */
  bcf_get_info_int32 (vcf_header, buffer, "CIPOS", &(p1->cipos), &(p1->cipos_len));

  p1->chromosome = (char *)bcf_seqname (vcf_header, buffer);
  p1->chromosome_len = strlen (p1->chromosome);
  p1->position = buffer->pos;
  p1->reference = program_config.reference;

  pthread_mutex_lock (&output_mutex);
  print_GenomePosition (p1);
  pthread_mutex_unlock (&output_mutex);

  free (p1->cipos);
  p1->cipos = NULL;
  p1->cipos_len = 0;
  
  /* Handle the second genome position.
   * ------------------------------------------------------------------------ */
  if (is_sv)
    {
      bcf_info_t *end_info = bcf_get_info (vcf_header, buffer, "END");
      bcf_info_t *chr2_info = bcf_get_info (vcf_header, buffer, "CHR2");

      if (end_info != NULL && chr2_info != NULL)
        {
          bcf_get_info_int32 (vcf_header, buffer, "CIEND", &(p2->cipos), &(p2->cipos_len));
          p2->chromosome = calloc (1, chr2_info->len + 1);
          p2->chromosome_len = chr2_info->len;

          if (p2->chromosome == NULL)
            {
              printf ("# ERROR allocating memory.\n");
              return;
            }

          memcpy (p2->chromosome, chr2_info->vptr, chr2_info->len);

          p2->position = end_info->v1.i;
          pthread_mutex_lock (&output_mutex);
          print_GenomePosition (p2);
          pthread_mutex_unlock (&output_mutex);

          /* The hash has been generated, and the data has been printed.  So, we
           * no longer need to have this data allocated.  Warning:  If you ever
           * implement a feature that does not use the cache of GenomePositions,
           * then remove this code and handle the free()'ing of this string in
           * the appropriate place. */
          free (p2->chromosome);
          p2->chromosome_len = 0;
        }
      else
        {
          /* This indicates we are missing data.  If it is an SV, we expect to
           * have a second position for the breakpoint.  If we haven't found it,
           * we need to look better (in the future). */
          p2->chromosome = "unknown";
          p2->chromosome_len = 7;
          p2->position = 0;
          p2->cipos = calloc (2, sizeof (int32_t));
          p2->cipos_len = 2;
          p2->reference = program_config.reference;

          pthread_mutex_lock (&output_mutex);
          print_GenomePosition (p2);
          pthread_mutex_unlock (&output_mutex);
        }

      free (p2->cipos);
      p2->cipos = NULL;
      p2->cipos_len = 0;
    }


  /* Handle the variant information.
   * ------------------------------------------------------------------------ */

  /* So, a StructuralVariant has all possible fields, so we use that
   * to allocate memory. */
  StructuralVariant v;
  initialize_StructuralVariant (&v);

  v.origin = o;

  if (is_sv)
    {
      v._obj_type = STRUCTURAL_VARIANT;
      v.position2 = p2;
    }
  else
    v._obj_type = VARIANT;

  v.position1 = p1;

  /* Set the 'quality' field.
   * ------------------------------------------------------------------------ */
  v.quality = buffer->qual;

  /* Make sure the FILTER field is available.
   * ------------------------------------------------------------------------ */
  if (!(buffer->unpacked & BCF_UN_FLT))
    bcf_unpack(buffer, BCF_UN_FLT);

  v.filters_len = buffer->d.n_flt;
  v.filters = buffer->d.flt;

  /* Set the 'type' field.
   * ------------------------------------------------------------------------ */
  if (svtype_info)
    {
      v.type = svtype_info->vptr;
      v.type_len = svtype_info->len;
    }

  /* Print the Variant.
   * ------------------------------------------------------------------------ */
  pthread_mutex_lock (&output_mutex);
  print_Variant ((Variant *)&v, vcf_header);
  pthread_mutex_unlock (&output_mutex);

  /* Clean up the memory.
  * ------------------------------------------------------------------------- */
  reset_GenomePosition (p1);
  reset_GenomePosition (p2);
  reset_StructuralVariant (&v);
}

void
handle_BND_record (UNUSED bcf_hdr_t *vcf_header, UNUSED bcf1_t *buffer)
{
  puts ("# BND records have not been implemented (yet).");
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
      GenomePosition p1;
      initialize_GenomePosition (&p1);
      GenomePosition p2;
      initialize_GenomePosition (&p2);

      /* A VCF file can contain all kinds of data.  Each is represented
       * a little different in the graph model. */
      bcf1_t *buffer = pack->buffers[i + (pack->thread_number * pack->jobs_to_run)];

      variant_type = bcf_get_variant_types (buffer);
      switch (variant_type)
        {
        case VCF_REF:    handle_REF_record (vcf_header, buffer); break;
        case VCF_MNP:    handle_MNP_record (vcf_header, buffer); break;
        case VCF_SNP:
          handle_SNP_record (vcf_header, buffer, &p1, origin);
          break;
        case VCF_BND:
        case VCF_INDEL:
        case VCF_OTHER:
          handle_OTHER_record (vcf_header, buffer, &p1, &p2, origin);
          break;
        default:
          puts ("# Encountered an unknown variant call type.");
          break;
        }
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

  VcfHeader h;
  initialize_VcfHeader (&h);

  int32_t i;
  for (i = 0; i < vcf_header->nhrec; i++)
    {
      int32_t j;
      h.type = vcf_header->hrec[i]->key;
      if (vcf_header->hrec[i]->value)
        {
          h.key = vcf_header->hrec[i]->key;
          h.key_len = strlen (vcf_header->hrec[i]->key);
          h.value = vcf_header->hrec[i]->value;
          h.value_len = strlen (vcf_header->hrec[i]->value);
          h.type = "generic";
          h.origin = origin;

          print_VcfHeader (&h);
          h.type = NULL;
        }

      /* for (j = 0; j < vcf_header->hrec->nkeys; j++) */
      /*   { */
      /*     if (!strcasecmp(key,hrec->keys[i])) return i; */
      /*   } */
    }

  /* int bcf_hrec_find_key(bcf_hrec_t *hrec, const char *key) */
  /* { */
  /*   int i; */
  /*   for (i=0; i<hrec->nkeys; i++) */
  /*     if ( !strcasecmp(key,hrec->keys[i]) ) return i; */
  /*   return -1; */
  /* } */
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
  program_config.use_faldo = false;
  program_config.graph_location = (char *)DEFAULT_GRAPH_LOCATION;
  program_config.reference = "unknown";
  program_config.threads = 2;
  program_config.jobs_per_thread = 250;

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
          { "use-faldo",         no_argument,       0, 'z' },
          { "reference-genome",  required_argument, 0, 'r' },
          { "threads",           required_argument, 0, 't' },
	  { "help",              no_argument,       0, 'h' },
	  { "version",           no_argument,       0, 'v' },
	  { 0,                   0,                 0, 0   }
	};

      while ( arg != -1 )
	{
	  /* Make sure to list all short options in the string below. */
	  arg = getopt_long (argc, argv, "i:f:g:k:r:t:zvh", options, &index);
          switch (arg)
            {
            case 'i': program_config.input_file = optarg;            break;
            case 'f': program_config.filter = optarg;                break;
            case 'k': program_config.keep = optarg;                  break;
            case 'g': program_config.graph_location = optarg;        break;
            case 'z': program_config.use_faldo = true;               break;
            case 'r': program_config.reference = optarg;             break;
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
      printf ("@prefix : <%s> .\n", program_config.graph_location);
      printf ("@prefix v: <%sVariant/> .\n", program_config.graph_location);
      printf ("@prefix p: <%sPosition/> .\n", program_config.graph_location);
      printf ("@prefix s: <%sSample/> .\n", program_config.graph_location);
      printf ("@prefix h: <%sVcfHeader/> .\n", program_config.graph_location);
      printf ("@prefix o: <%sOrigin/> .\n", program_config.graph_location);

      if (program_config.use_faldo)
        printf ("@prefix faldo: <http://biohackathon.org/resource/faldo#> .\n");

      puts ("");

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
