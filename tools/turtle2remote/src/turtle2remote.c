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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <getopt.h>
#include <termios.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <curl/curl.h>

static void
show_help (void)
{
  puts ("\nAvailable options:\n"
        "  --input-file,        -i  The file to upload to a remote endpoint.\n"
        "  --remote-endpoint,   -r  The remote endpoint to upload data to.\n"
        "  --graph-iri,         -g  The graph IRI to use.\n"
        "  --username,          -u  The username to authenticate with.\n"
        "  --password,          -p  The password to authenticate with.\n"
	"  --version,           -v  Show versioning information.\n"
	"  --help,              -h  Show this message.\n");
}

static void
show_version (void)
{
  puts ("Version: 0.0.1\n");
}

static int32_t
handle_file_error (const char *input_file)
{
  fprintf (stderr, "Error: Could not read file '%s'\n", input_file);
  return 1;
}

static int32_t
handle_curl_error (void)
{
  fprintf (stderr, "Error: Could not initialize cURL\n");
  return 1;
}

static int32_t
handle_masked_password_error (void)
{
  fprintf (stderr, "Error: Could not mask the password input.\n");
  return 1;
}

static int32_t
handle_auth_setup_failure (void)
{
  fprintf (stderr, "Error: Could not set up authentication.\n");
  return 1;
}

static int32_t
handle_username_entry_error (void)
{
  fprintf (stderr, "Error: Could not set username.\n");
  return 1;
}

static int32_t
handle_password_entry_error (void)
{
  fprintf (stderr, "Error: Could not set password.\n");
  return 1;
}

bool
assemble_auth_string (char *username, char *password,
                      int32_t username_len, int32_t password_len,
                      char *buffer)
{
  int32_t buffer_len = username_len + password_len + 2;
  memset (buffer, '\0', buffer_len);

  if (snprintf (buffer, buffer_len, "%s:%s",
                username, password) != (buffer_len - 1))
    {
      memset (buffer, '\0', buffer_len);
      return false;
    }
  else
    return true;
}

int
main (int argc, char **argv)
{
  /*--------------------------------------------------------------------------.
   | PROCESS COMMAND-LINE OPTIONS                                             |
   '--------------------------------------------------------------------------*/

  char *input_file = NULL;
  char *remote_url = NULL;
  char username[255];
  char password[255];
  char *graph_iri = NULL;
  bool from_stdin = false;

  memset (username, '\0', 255);
  memset (password, '\0', 255);

  if (argc > 1)
    {
      int arg = 0;
      int index = 0;

      /* Program options
       * ------------------------------------------------------------------- */
      static struct option options[] =
	{
          { "input-file",        required_argument, 0, 'i' },
          { "remote-endpoint",   required_argument, 0, 'r' },
          { "graph-iri",         required_argument, 0, 'g' },
          { "from-stdin",        no_argument,       0, 's' },
          { "username",          optional_argument, 0, 'u' },
          { "password",          optional_argument, 0, 'p' },
	  { "help",              no_argument,       0, 'h' },
	  { "version",           no_argument,       0, 'v' },
	  { 0,                   0,                 0, 0   }
	};

      while ( arg != -1 )
	{
	  /* Make sure to list all short options in the string below. */
	  arg = getopt_long (argc, argv, "i:r:u:p:g:svh", options, &index);
          switch (arg)
            {
            case 'i': input_file = optarg;            break;
            case 'r': remote_url = optarg;            break;
            case 's': from_stdin = true;              break;
            case 'u':
              if (optarg) strncpy (username, optarg, 255);
              break;
            case 'p':
              if (optarg) strncpy (password, optarg, 255);
              break;
            case 'g': graph_iri = optarg;             break;
            case 'h': show_help (); return 0;         break;
            case 'v': show_version (); return 0;      break;
            }
        }
    }
  else
    {
      show_help ();
      return 0;
    }

  if ((input_file || from_stdin) && remote_url && graph_iri)
    {
      /* Set 'input_fp' to either a file handle or stdin.
       * -------------------------------------------------------------------- */
      FILE *input_fp = NULL;

      if (from_stdin)
        input_fp = stdin;
      else
        {
          input_fp = fopen (input_file, "r");
          if (input_fp == NULL)
            return handle_file_error (input_file);
        }

      /* Set up cURL.
       * -------------------------------------------------------------------- */
      curl_global_init (CURL_GLOBAL_ALL);

      CURL *curl = NULL;
      CURLcode result;
      
      /* We can only send one file with the command-line options, so running
       * in synchronous mode is OK. */
      curl = curl_easy_init ();
      if (curl == NULL)
        return handle_curl_error ();

      /* Set the input stream to upload. */
      curl_easy_setopt (curl, CURLOPT_READDATA, input_fp);

      /* Make the request a PUT request, by enabling uploading. */
      curl_easy_setopt (curl, CURLOPT_UPLOAD, 1L);

      /* Specify the target URL.
       * --------------------------------------------------------------------
       * This URL is the concatenation of the '--remote-endpoint' option
       * (remote_url), and the '--graph-iri' option (graph_iri).
       */
      int32_t remote_url_len = strlen (remote_url);
      int32_t graph_iri_len = strlen (graph_iri);

      char *graph_iri_encoded = curl_easy_escape (curl, graph_iri, graph_iri_len);
      if (graph_iri_encoded == NULL)
        return handle_curl_error ();

      int32_t graph_iri_encoded_len = strlen (graph_iri_encoded);

      /* The 31 comes from the '/sparql-graph-crud-auth?graph=' part plus
       * one terminating byte. */
      int32_t total_url_len = remote_url_len + graph_iri_encoded_len + 31;
      char total_url[total_url_len];

      if (snprintf (total_url, total_url_len,
                    "%s/sparql-graph-crud-auth?graph=%s",
                    remote_url, graph_iri_encoded) == total_url_len - 1)
        curl_easy_setopt (curl, CURLOPT_URL, remote_url);
      else
        return handle_curl_error ();

      curl_free (graph_iri_encoded);
      graph_iri_encoded = NULL;
      graph_iri_encoded_len = 0;

      /* Set HTTP headers
       * --------------------------------------------------------------------
       * We can only determine the file size when we are actually using a
       * file.  For 'stdin' we need to use the 'chunked' transfer mode.
       */
      struct curl_slist *headers = NULL;

      headers = curl_slist_append (headers, "User-Agent: turtle2remote");
      headers = curl_slist_append (headers, "Content-Type: text/turtle");

      if (!from_stdin)
        {
          struct stat file_info;
          fstat(fileno (input_fp), &file_info);

          curl_easy_setopt (curl, CURLOPT_INFILESIZE_LARGE,
                            (curl_off_t)file_info.st_size);
        }
      else
        {
          /* Since we do not read the contents from stdin, but let cURL do that,
           * we cannot know the Content-Length.  We must therefore use the
           * 'chunked' Transfer-Encoding, which allows sending data in chunks,
           * which suits us very well here. */
          headers = curl_slist_append (headers, "Transfer-Encoding: chunked");
        }

      curl_easy_setopt (curl, CURLOPT_HTTPHEADER, headers);

      /* Handle authentication.
       * --------------------------------------------------------------------
       * 
       * Virtuoso uses Digest authentication.  We may need to
       * revisit this authentication mechanism when using other
       * SPARQL endpoints.
       */
      curl_easy_setopt (curl, CURLOPT_HTTPAUTH, CURLAUTH_DIGEST);

      int32_t username_len = 0;
      int32_t password_len = 0;
      int32_t auth_str_len = 0;

      if (username[0] == 0 || password[0] == 0)
        {
          /* Get the username
           * ---------------------------------------------------------------- */
          if (username[0] == 0)
            {
              printf ("Username: ");
              if (fgets (username, 255, stdin) == NULL)
                return handle_username_entry_error ();

              /* Strip the newline character if there is one. */
              char *newline = strchr (username, '\n');
              if (newline) *newline = '\0';
            }

          /* Get the password without echoing to stdout.
           * ---------------------------------------------------------------- */
          if (password[0] == 0)
            {
              struct termios old, new;

              /* Turn echoing off and fail if we can’t. */
              if (tcgetattr (fileno (stdin), &old) != 0)
                return handle_masked_password_error ();

              new = old;
              new.c_lflag &= ~ECHO;

              if (tcsetattr (fileno (stdin), TCSAFLUSH, &new) != 0)
                return handle_masked_password_error ();

              /* Read the password. */
              printf ("Password: ");
              if (fgets (password, 255, stdin) == NULL)
                return handle_password_entry_error ();

              /* Strip the newline character if there is one. */
              char *newline = strchr (password, '\n');
              if (newline) *newline = '\0';

              /* Reset the terminal. */
              (void) tcsetattr (fileno (stdin), TCSAFLUSH, &old);
              puts ("");
            }
        }

      username_len = strlen (username);
      password_len = strlen (password);
      auth_str_len = username_len + password_len + 2;
      char auth_str[auth_str_len];

      if (assemble_auth_string (username, password, username_len,
                                password_len, auth_str))
        curl_easy_setopt (curl, CURLOPT_USERPWD, auth_str);
      else
        return handle_auth_setup_failure ();

      /* This buffer contains sensitive data, so we need to clear it
       * as soon as possible. */
      memset (auth_str, '\0', auth_str_len);
      auth_str_len = 0;
      username_len = 0;
      password_len = 0;

      /* Send the data.
       * -------------------------------------------------------------------- */
      result = curl_easy_perform (curl);

      /* Check for errors */
      if (result != CURLE_OK)
        fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(result));
      else
        {
          /* Display some metrics.
           * ---------------------------------------------------------------- */
          CURLcode info;
          double uploaded;
          double downloaded;
          double speed;

          puts ("");

          info = curl_easy_getinfo (curl, CURLINFO_SIZE_UPLOAD, &uploaded);
          if (!info)
            printf ("Uploaded:     %.0f bytes\n", uploaded);

          info = curl_easy_getinfo (curl, CURLINFO_SIZE_DOWNLOAD, &downloaded);
          if (!info)
            printf ("Downloaded:   %.0f bytes\n", downloaded);

          info = curl_easy_getinfo (curl, CURLINFO_SPEED_UPLOAD, &speed);
          if (!info)
            printf ("Upload speed: %.0f b/s\n", speed);

          puts ("");
        }

      long response_code;
      curl_easy_getinfo (curl, CURLINFO_RESPONSE_CODE, &response_code);

      switch (response_code)
        {
        case 200:
          printf ("The data was already in the database.\n");
          break;
        case 201:
          printf ("The data has been added to the database.\n");
          break;
        case 204:
          printf ("No data has been added to the database.\n");
          break;
        default:
          printf ("Could not submit data to remote endpoint (%ld).\n",
                  response_code);
          break;
        }
      
      curl_slist_free_all (headers);
      curl_easy_cleanup (curl);

      if (!from_stdin)
        fclose (input_fp);

      curl_global_cleanup();
    }
  else
    {
      puts("The following information is required:");
      if (!input_file)
        puts (" * Specify an input file with the '-i' option.");

      if (!remote_url)
        puts (" * Specify a remote endpoint with the '-r' option.");

      if (!graph_iri)
        puts (" * Specify a graph IRI with the '-g' option.");

      puts ("\nUse '--help' to display all available options.\n");
    }

  return 0;
}
