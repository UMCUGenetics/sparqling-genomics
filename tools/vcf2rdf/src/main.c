/*
 * Copyright (C) 2018  Roel Janssen <roel@gnu.org>
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
#include <string.h>
#include <time.h>
#include <raptor2.h>
#include <htslib/vcf.h>

#ifdef ENABLE_MTRACE
#include <mcheck.h>
#endif

#include "ui.h"
#include "helper.h"
#include "runtime_configuration.h"
#include "vcf_header.h"
#include "vcf_variants.h"
#include "ontology.h"

int
main (int argc, char **argv)
{
#ifdef ENABLE_MTRACE
  mtrace ();
#endif

  /* Initialize the run-time configuration.
   * ------------------------------------------------------------------------ */
  if (!runtime_configuration_init ()) return 1;

  /* Process command-line arguments.
   * ------------------------------------------------------------------------ */
  if (argc > 1)
    ui_process_command_line (argc, argv);
  else
    ui_show_help ();

  /* Initialize the Redland run-time configuration.
   * ------------------------------------------------------------------------ */
  if (!runtime_configuration_redland_init ()) return 1;

  /* Read the input file.
   * ------------------------------------------------------------------------ */
  if (config.input_file)
    {
      ui_show_missing_options_warning ();
      hts_verbose = 0;

      /* Determine whether the input file is a VCF or compressed VCF.
       * -------------------------------------------------------------------- */
      int32_t input_file_len = strlen (config.input_file);
      bool is_vcf = false;
      bool is_gzip_vcf = false;
      bool is_bcf = false;
      bool is_gzip_bcf = false;

      if (input_file_len > 3)
        {
          is_vcf = !strcmp (config.input_file + input_file_len - 3, "vcf");
          is_bcf = !strcmp (config.input_file + input_file_len - 3, "bcf");
        }

      if (!is_vcf && !is_bcf && input_file_len > 6)
        {
          is_gzip_vcf = !strcmp (config.input_file + input_file_len - 6, "vcf.gz");
          is_gzip_bcf = !strcmp (config.input_file + input_file_len - 6, "bcf.gz");
        }

      if (!(is_bcf || is_gzip_bcf || is_vcf || is_gzip_vcf))
        return ui_print_file_format_error ();

      /* Prepare the buffers needed to read the VCF file.
       * -------------------------------------------------------------------- */
      bcf_hdr_t *vcf_header = NULL;
      htsFile   *vcf_stream = NULL;

      if (is_vcf)           vcf_stream = hts_open (config.input_file, "r");
      else if (is_gzip_vcf) vcf_stream = hts_open (config.input_file, "rz");
      else if (is_bcf)      vcf_stream = hts_open (config.input_file, "rbu");
      else if (is_gzip_bcf) vcf_stream = hts_open (config.input_file, "rb");

      if (!vcf_stream)
        return ui_print_vcf_file_error (config.input_file);

      /* Read the VCF header.
       * -------------------------------------------------------------------- */
      vcf_header = bcf_hdr_read (vcf_stream);
      if (!vcf_header)
        {
          hts_close (vcf_stream);
          return ui_print_vcf_header_error (config.input_file);
        }

      unsigned char *file_hash = helper_get_hash_from_file (config.input_file);
      if (!file_hash) return 1;

      raptor_statement *stmt;
      raptor_term *node_filename;

      node_filename = term (PREFIX_BASE, (char *)file_hash);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_RDF, "type");
      stmt->object    = term (PREFIX_BASE, "Origin");
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_BASE, "convertedBy");
      stmt->object    = term (PREFIX_BASE, "vcf2rdf");
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_BASE, "vcf2rdf");
      stmt->predicate = term (PREFIX_OWL, "versionInfo");
      stmt->object    = literal (VERSION, XSD_STRING);
      register_statement (stmt);
      stmt = NULL;

      /* Process the header. */
      process_header (vcf_header, file_hash);

      if (!config.header_only)
        {
          /* Process variant calls. */
          bcf1_t *buffer = bcf_init ();
          int32_t counter = 0;
          time_t rawtime = 0;
          char time_str[20];

          if (config.show_progress_info)
            {
              fprintf (stderr, "[ PROGRESS ] %-20s%-20s\n",
                       "Variants", "Time");
              fprintf (stderr, "[ PROGRESS ] ------------------- "
                       "------------------- -------------------\n");
              while (bcf_read (vcf_stream, vcf_header, buffer) == 0)
                {
                  process_variant (vcf_header, buffer, file_hash);
                  if (counter % 50000 == 0)
                    {
                      rawtime = time (NULL);
                      strftime (time_str, 20, "%Y-%m-%d %H:%M:%S", localtime (&rawtime));
                      fprintf(stderr, "[ PROGRESS ] %-20d%-20s\n", counter, time_str);
                    }

                  counter++;
                }

              fprintf (stderr,
                       "[ PROGRESS ] \n"
                       "[ PROGRESS ] Total number variants: %d\n", counter);
            }
          else
            {
              while (bcf_read (vcf_stream, vcf_header, buffer) == 0)
                process_variant (vcf_header, buffer, file_hash);
            }

          bcf_destroy (buffer);
        }

      /* Clean up. */
      raptor_free_term (node_filename);
      free (file_hash);
      bcf_hdr_destroy (vcf_header);
      hts_close (vcf_stream);
    }

  runtime_configuration_free ();

#ifdef ENABLE_MTRACE
  muntrace ();
#endif

  return 0;
}
