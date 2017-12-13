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
#include "helper.h"
#include "ui.h"

#ifdef __GNUC__
#define UNUSED __attribute__((__unused__))
#else
#define UNUSED
#endif

extern RuntimeConfiguration program_config;
extern int hts_verbose;
static pthread_mutex_t output_mutex;
const char DEFAULT_GRAPH_LOCATION[] = "http://localhost:8890/";

/*----------------------------------------------------------------------------.
 | HANDLERS FOR SPECIFIC VCF RECORD TYPES                                     |
 '----------------------------------------------------------------------------*/

void get_info_value_int32_t (bcf_hdr_t *header, bcf1_t *buffer,
                             const char *field, int32_t *destination)
{
  void *buf = NULL;
  int32_t buf_len = 0;

  if (bcf_get_info_int32 (header, buffer, field, &buf, &buf_len) > 0)
    {
      *destination = ((int32_t *)buf)[0];
      free (buf); buf = NULL; buf_len = 0;
    }
}

void
handle_record (Origin *origin, bcf_hdr_t *header, bcf1_t *buffer)
{
  /* Handle the program options for leaving out FILTER fields.
   * ------------------------------------------------------------------------ */
  if (program_config.filter &&
      bcf_has_filter (header, buffer, program_config.filter) == 1)
    return;

  if (program_config.keep &&
      bcf_has_filter (header, buffer, program_config.keep) != 1)
    return;

  Variant variant;
  variant_initialize (&variant, bcf_get_variant_types (buffer));
  variant.origin = origin;

  /* Unpack up and including the ALT field. */
  bcf_unpack (buffer, BCF_UN_STR);

  if (!variant_gather_data (&variant, header, buffer))
    fprintf (stderr, "# Couldn't gather variant data.\n");

  char *svtype = NULL;
  int32_t svtype_len = 0;
  bcf_get_info_string (header, buffer, "SVTYPE", &svtype, &svtype_len);

  void *buf = NULL;
  int32_t buf_len = 0;

  /* Gather the positions.
   * ------------------------------------------------------------------------ */
  FaldoExactPosition start_position, end_position;
  faldo_exact_position_initialize (&start_position);
  variant.start_position = &start_position;

  faldo_exact_position_initialize (&end_position);
  variant.end_position = &end_position;

  /* HTSlib uses 0-based positions, while in the VCF 1-based position are used.
   * Therefore we need to add one to the position here. */
  start_position.position = buffer->pos + 1;
  start_position.chromosome = (char *)bcf_seqname (header, buffer);
  start_position.chromosome_len = strlen (start_position.chromosome);

  /* For BND, the chromosome for the end position can be different. */
  BndProperties properties;
  bnd_properties_init (&properties);

  if (svtype != NULL && !strcmp (svtype, "BND"))
    {
      if (!parse_properties (&properties, variant.alternative,
                             strlen (variant.alternative)))
        puts ("# WARNING: Failed to parse complex rearrangement.");
      else
        {
          variant.is_complex_rearrangement = true;
          variant.is_reversed = properties.is_reversed;
          variant.is_left_of_ref = properties.is_left_of_ref;

          end_position.chromosome = properties.chromosome;
          end_position.position = properties.position;
        }
    }
  else
    {
      /* For anything other than complex rearrangements, we can assume the
       * chromosome of the start position is the same as the chromosome for the
       * end position. */
      end_position.chromosome = start_position.chromosome;

      /* Usually, an END info-field is provided. */
      if (bcf_get_info_int32 (header, buffer, "END", &buf, &buf_len) > 0)
        end_position.position = ((int32_t *)buf)[0];
      /* Without the END info-field, we cannot know where the SV ends. */
      else
        end_position.position = -1;

      free (buf);
      buf = NULL;
      buf_len = 0;
    }

  /* In case SVTYPE was allocated, clean it up here. */
  if (svtype != NULL)
    {
      free (svtype);
      svtype = NULL;
    }

  if (end_position.chromosome != NULL)
    end_position.chromosome_len = strlen (end_position.chromosome);

  /* When the positions are not on the same chromosome, our length indication
   * does not make any sense.  What also doesn't make sense is a length of 0. */
  if (strcmp (end_position.chromosome, start_position.chromosome))
    variant.length = 0;
  else if (end_position.position != 0 && start_position.position != 0)
    {
      /* In the case of insertions in Manta, the END property indicates the
       * end position of the reference genome, while the SVLEN property
       * indicates the end of the insertion. So the length of the insertion
       * is SVLEN - POSITION, rather than END - POSITION. */
      if (bcf_get_info_int32 (header, buffer, "SVLEN", &buf, &buf_len) > 0)
        variant.length = start_position.position + ((int32_t *)buf)[0];
      else
        variant.length = abs(end_position.position - start_position.position);
    }

  /* Handle the confidence intervals.
   * ------------------------------------------------------------------------ */
  FaldoExactPosition cipos_before, cipos_after, ciend_before, ciend_after;
  faldo_exact_position_initialize (&cipos_before);
  faldo_exact_position_initialize (&cipos_after);
  faldo_exact_position_initialize (&ciend_before);
  faldo_exact_position_initialize (&ciend_after);

  FaldoInBetweenPosition cipos, ciend;
  faldo_in_between_position_initialize (&cipos);
  faldo_in_between_position_initialize (&ciend);

  bool has_cipos;
  has_cipos = determine_confidence_interval (variant.start_position, "CIPOS",
                                             &cipos_before, &cipos_after,
                                             header, buffer);

  bool has_ciend;
  has_ciend = determine_confidence_interval (variant.end_position, "CIEND",
                                             &ciend_before, &ciend_after,
                                             header, buffer);

  cipos.before = &cipos_before;
  cipos.after = &cipos_after;
  ciend.before = &cipos_before;
  ciend.after = &cipos_after;

  variant.cipos = &cipos;
  variant.ciend = &ciend;

  /* Probe for more fields/information.
   *
   * Please note that these properties may not be available in the VCF.  So
   * write your code in a way that allows for these values to be missing.
   * ------------------------------------------------------------------------ */
  get_info_value_int32_t (header, buffer, "MAPQ", &(variant.mapq));
  get_info_value_int32_t (header, buffer, "PE", &(variant.paired_end_support));
  get_info_value_int32_t (header, buffer, "SR", &(variant.split_read_support));

  if (bcf_get_info_float (header, buffer, "SRQ", &buf, &buf_len) > 0)
    {
      variant.split_read_consensus_alignment_quality = ((float *)buf)[0];
      free (buf); buf = NULL; buf_len = 0;
    }

  get_info_value_int32_t (header, buffer, "RC", &(variant.read_count));
  get_info_value_int32_t (header, buffer, "DR", &(variant.hq_reference_pairs));
  get_info_value_int32_t (header, buffer, "DV", &(variant.hq_variant_pairs));
  get_info_value_int32_t (header, buffer, "RR", &(variant.hq_ref_junction_reads));
  get_info_value_int32_t (header, buffer, "RV", &(variant.hq_var_junction_reads));

  bcf_get_info_string (header, buffer, "CIGAR", &(variant.cigar), &(variant.cigar_len));
  get_info_value_int32_t (header, buffer, "PAIR_COUNT", &(variant.pair_count));

  /* Output the gathered information.
   * ------------------------------------------------------------------------ */
  pthread_mutex_lock (&output_mutex);
  if (has_cipos)
    {
      faldo_position_print ((FaldoBaseType *)(variant.cipos->before));
      faldo_position_print ((FaldoBaseType *)(variant.cipos->after));
      faldo_position_print ((FaldoBaseType *)(variant.cipos));
    }
  if (has_ciend)
    {
      faldo_position_print ((FaldoBaseType *)(variant.ciend->before));
      faldo_position_print ((FaldoBaseType *)(variant.ciend->after));
      faldo_position_print ((FaldoBaseType *)(variant.ciend));
    }
  faldo_position_print ((FaldoBaseType *)(variant.start_position));
  faldo_position_print ((FaldoBaseType *)(variant.end_position));
  variant_print (&variant, header);
  pthread_mutex_unlock (&output_mutex);

  if (properties.chromosome != NULL)
    {
      free (properties.chromosome);
      properties.chromosome = NULL;
    }

  faldo_position_reset ((FaldoBaseType *)(variant.cipos->before), FALDO_EXACT_POSITION);
  faldo_position_reset ((FaldoBaseType *)(variant.cipos->after), FALDO_EXACT_POSITION);
  faldo_position_reset ((FaldoBaseType *)(variant.cipos), FALDO_IN_BETWEEN_POSITION);
  faldo_position_reset ((FaldoBaseType *)(variant.ciend->before), FALDO_EXACT_POSITION);
  faldo_position_reset ((FaldoBaseType *)(variant.ciend->after), FALDO_EXACT_POSITION);
  faldo_position_reset ((FaldoBaseType *)(variant.ciend), FALDO_IN_BETWEEN_POSITION);
  faldo_position_reset ((FaldoBaseType *)(variant.start_position), FALDO_EXACT_POSITION);
  faldo_position_reset ((FaldoBaseType *)(variant.end_position), FALDO_EXACT_POSITION);
  variant_reset (&variant);
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
  program_config.graph_location = (char *)DEFAULT_GRAPH_LOCATION;
  program_config.threads = 2;
  program_config.jobs_per_thread = 500;
  program_config.no_header = false;

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
          { "publish-to",        required_argument, 0, 'p' },
          { "reference",         required_argument, 0, 'r' },
          { "caller",            required_argument, 0, 'c' },
          { "threads",           required_argument, 0, 't' },
          { "no-header",         no_argument,       0, 'n' },
	  { "help",              no_argument,       0, 'h' },
	  { "version",           no_argument,       0, 'v' },
	  { 0,                   0,                 0, 0   }
	};

      while ( arg != -1 )
	{
	  /* Make sure to list all short options in the string below. */
	  arg = getopt_long (argc, argv, "i:f:p:k:r:c:t:nvh", options, &index);
          switch (arg)
            {
            case 'c': program_config.caller = optarg;                break;
            case 'f': program_config.filter = optarg;                break;
            case 'p': program_config.graph_location = optarg;        break;
            case 'i': program_config.input_file = optarg;            break;
            case 'k': program_config.keep = optarg;                  break;
            case 'r': program_config.reference = optarg;             break;
            case 't': program_config.threads = atoi(optarg);         break;
            case 'n': program_config.no_header = true;               break;
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
      if (!strcmp (program_config.reference, "unknown"))
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
      if (! program_config.no_header)
        {
          puts ("@prefix vcf:     <http://semweb.op.umcutrecht.nl/vcf/> .\n"
                "@prefix smo:     <http://semweb.op.umcutrecht.nl/smo/> .\n"
                "@prefix faldo:   <http://biohackathon.org/resource/faldo#> .\n"
                "@prefix rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
                "@prefix rdfs:    <http://www.w3.org/2000/01/rdf-schema#> .\n"
                "@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .");

          printf ("@prefix :        <%s> .\n",                   program_config.graph_location);
          printf ("@prefix v:       <%sVariant/> .\n",           program_config.graph_location);
          printf ("@prefix p:       <%sPosition/> .\n",          program_config.graph_location);
          printf ("@prefix ep:      <%sExactPosition/> .\n",     program_config.graph_location);
          printf ("@prefix ip:      <%sInBetweenPosition/> .\n", program_config.graph_location);
          printf ("@prefix rp:      <%sRangedPosition/> .\n",    program_config.graph_location);
          printf ("@prefix s:       <%sSample/> .\n",            program_config.graph_location);
          printf ("@prefix h:       <%sVcfHeader/> .\n",         program_config.graph_location);
          printf ("@prefix o:       <%sOrigin/> .\n",            program_config.graph_location);

          /* There seem to be slight differences between the way URIs for GRCh38
           * and GRCh37.  Also, the GRCh37 is only available for download from their FTP
           * server.  So it cannot be queried by their own web-based ontology browser. */
          if (!strcmp (program_config.reference, "GRCh37") ||
              !strcmp (program_config.reference, "grch37"))
            {
              program_config.reference = "grch37";
              puts ("@prefix grch37:  <http://rdf.ebi.ac.uk/resource/ensembl/83/chromosome:GRCh37:> .");
            }
          else if (!strcmp (program_config.reference, "GRCh38") ||
                   !strcmp (program_config.reference, "grch38"))
            {
              program_config.reference = "grch38";
              puts ("@prefix grch38:  <http://rdf.ebi.ac.uk/resource/ensembl/90/homo_sapiens/GRCh38/> .");
            }

          puts ("");
        }

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
              int32_t full_path_len = strlen (cwd) + strlen (program_config.input_file) + 2;
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
          if (queue >= (program_config.jobs_per_thread * program_config.threads - 1))
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
        for (j = 0; j < queue; j++)
          handle_record (&o, vcf_header, tbuffers[j]);

      for (j = 0; j < tbuffers_len; j++)
        bcf_destroy (tbuffers[j]);

      bcf_hdr_destroy (vcf_header);
      hts_close (vcf_stream);
    }

  return 0;
}
