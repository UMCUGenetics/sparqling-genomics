/*
 * Copyright (C) 2019  Roel Janssen <roel@gnu.org>
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
#include <gcrypt.h>
#include <zlib.h>

#ifdef ENABLE_MTRACE
#include <mcheck.h>
#endif

#include "ui.h"
#include "helper.h"
#include "runtime_configuration.h"
#include "xml.h"
#include "ontology.h"
#include "id.h"
#include "list.h"

int
main (int argc, char **argv)
{
#ifdef ENABLE_MTRACE
  mtrace ();
#endif

  /* Initialize libgcrypt. */
  if (!gcry_check_version (GCRYPT_VERSION))
    {
      fputs ("libgcrypt version mismatch\n", stderr);
      exit (2);
    }

  /* We are not dealing with passwords or confidential crypto in this
   * program. Therefore we don't need "secure memory". */
  gcry_control (GCRYCTL_DISABLE_SECMEM, 0);
  gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);

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
  if (config.input_file || config.input_from_stdin)
    {
      /* Initialize the Redland run-time configuration.
       * -------------------------------------------------------------------- */
      if (!runtime_configuration_redland_init ()) return 1;

      ui_show_missing_options_warning ();

      /* Open a file stream.
       * -------------------------------------------------------------------- */

      int32_t input_file_len = 0;
      if (!(config.input_from_stdin || config.input_file == NULL))
        input_file_len = strlen (config.input_file);

      if (input_file_len == 0)
        config.input_from_stdin = true;

      gzFile stream;
      if (config.input_from_stdin)
        stream = gzdopen (fileno(stdin), "r");
      else
        stream = gzopen (config.input_file, "r");

      if (!stream)
        return ui_print_file_error (config.input_file);

      /* Get the file hash.
       * -------------------------------------------------------------------- */

      unsigned char *file_hash = NULL;
      if (!config.user_hash && !config.input_from_stdin)
        file_hash = helper_get_hash_from_file (config.input_file);
      else if (!config.user_hash && config.input_from_stdin)
        {
          unsigned char buf[32];
          memset (buf, '\0', 32);
          file_hash = calloc (65, sizeof (unsigned char));
          if (!file_hash) return 1;

          gcry_randomize (buf, 32, GCRY_VERY_STRONG_RANDOM);
          if (! get_pretty_hash (buf, 32, file_hash))
            return 1;
        }
      else
        file_hash = (unsigned char *)config.user_hash;

      if (!file_hash) return 1;

      raptor_statement *stmt;
      raptor_term *node_filename;

      config.origin_hash = (char *)file_hash;
      node_filename = term (PREFIX_ORIGIN, (char *)file_hash);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_RDF, "#type");
      stmt->object    = term (PREFIX_MASTER, "Origin");
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, "sha256sum");
      stmt->object    = literal ((char *)file_hash, XSD_STRING);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, "convertedBy");
      stmt->object    = term (PREFIX_MASTER, "xml2rdf-" VERSION);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = term (PREFIX_MASTER, "xml2rdf-" VERSION);
      stmt->predicate = term (PREFIX_OWL, "#versionInfo");
      stmt->object    = literal (VERSION, XSD_STRING);
      register_statement (stmt);

      stmt = raptor_new_statement (config.raptor_world);
      stmt->subject   = raptor_term_copy (node_filename);
      stmt->predicate = term (PREFIX_MASTER, "filename");
      stmt->object    = literal (config.input_file, XSD_STRING);
      register_statement (stmt);
      stmt = NULL;

      /* Setup and invoke the XML parser. 
       * -------------------------------------------------------------------
       *
       * We read the data in chunks so that we can process large files.
       * The ‘buffer’ determines the chunk size.
       */
      char buffer[4096];
      int bytes_read = 0;
      xmlSAXHandler handler;
      xmlParserCtxtPtr ctx;

      handler = make_sax_handler ();
      ctx = xmlCreatePushParserCtxt (&handler, NULL, buffer, bytes_read, NULL);

      while ((bytes_read = gzfread (buffer, 1, sizeof (buffer), stream)) > 0)
        {
          if (xmlParseChunk (ctx, buffer, bytes_read, 0))
            {
              xmlParserError(ctx, "xmlParseChunk");
              break;
            }
        }

      xmlParseChunk (ctx, buffer, 0, 1);
      xmlFreeParserCtxt (ctx);

      gzclose (stream);

      /* Clean up. */
      raptor_free_term (node_filename);
      runtime_configuration_free ();

      if (!config.user_hash) free (file_hash);
    }

#ifdef ENABLE_MTRACE
  muntrace ();
#endif

  return 0;
}
