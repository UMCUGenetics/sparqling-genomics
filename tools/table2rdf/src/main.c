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
#include "ontology.h"
#include "table.h"

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

  /* Read the input file.
   * ------------------------------------------------------------------------ */
  if (config.input_file)
    {
      /* Initialize the Redland run-time configuration.
       * -------------------------------------------------------------------- */
      if (!runtime_configuration_redland_init ()) return 1;

      ui_show_missing_options_warning ();

      FILE *stream = fopen (config.input_file, "r");
      if (!stream)
        return ui_print_file_error (config.input_file);

      unsigned char *file_hash = helper_get_hash_from_file (config.input_file);
      if (!file_hash) return 1;

      raptor_statement *stmt;
      raptor_term *node_filename;

      node_filename = term (PREFIX_BASE, (char *)file_hash);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_RDF, "#type");
      stmt->object    = term (PREFIX_BASE, "Origin");
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_BASE, "convertedBy");
      stmt->object    = term (PREFIX_BASE, "table2rdf");
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_BASE, "table2rdf");
      stmt->predicate = term (PREFIX_OWL, "#versionInfo");
      stmt->object    = literal (VERSION, XSD_STRING);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_BASE, "filename");
      stmt->object    = literal (config.input_file, XSD_STRING);
      register_statement (stmt);

      if (config.sample_name != NULL)
        {
          stmt = raptor_new_statement (config.raptor_world);
          stmt->subject   = term (PREFIX_SAMPLE, config.sample_name);
          stmt->predicate = term (PREFIX_BASE, "foundIn");
          stmt->object    = raptor_term_copy (node_filename);
          register_statement (stmt);
        }

      stmt = NULL;

      /* Process the header. */
      table_hdr_t *table = process_header (stream, file_hash, config.input_file);

      if (config.show_progress_info)
        {
          int32_t counter = 0;
          time_t rawtime = 0;
          char time_str[20];

          fprintf (stderr, "[ PROGRESS ] %-20s%-20s\n",
                   "Rows", "Time");
          fprintf (stderr, "[ PROGRESS ] ------------------- "
                   "------------------- -------------------\n");
          while (!feof (stream))
            {
              process_row (table, stream, file_hash, config.input_file);
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
                   "[ PROGRESS ] Total number rows: %d\n", counter);
        }
      else
        {
          while (!feof (stream))
            process_row (table, stream, file_hash, config.input_file);
        }

      uint32_t index = 0;
      for (; index < table->keys_len; index++)
        {
          free (table->column_ids[index]);
          free (table->keys[index]);
        }

      free (table->keys);
      free (table->column_ids);
      free (table);

      /* Clean up. */
      raptor_free_term (node_filename);
      runtime_configuration_free ();

      free (file_hash);
      fclose (stream);
    }

#ifdef ENABLE_MTRACE
  muntrace ();
#endif

  return 0;
}
