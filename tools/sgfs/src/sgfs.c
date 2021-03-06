/*
 * Copyright (C) 2019, 2020  Roel Janssen <roel@gnu.org>
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

#include "config.h"
#include <stdio.h>
#include <getopt.h>
#include <time.h>
#include <libguile.h>

/* GLOBAL STATE
 * --------------------------------------------------------------------------
 * The callback mechanisms from FUSE don't give much room for passing a
 * global state along.  So we must make sure to initialize the following
 * variables before spawning new threads.
 */
SCM log_error            = NULL;
SCM log_debug            = NULL;
SCM directories_for_path = NULL;
SCM attributes_for_path  = NULL;
SCM is_directory         = NULL;
char *mountpoint         = NULL;
char token[85];
char *endpoint           = NULL;

void
show_help (void)
{
  puts ("This is sgfs:\n"
	"  --debug-log,              -d  File to write debug messages to.\n"
        "  --endpoint,               -E  The endpoint to communicate with.\n"
	"  --error-log,              -e  File to write error messages to.\n"
	"  --help,                   -h  Show this message.\n"
	"  --mountpoint,             -m  Directory to mount SGSF.\n"
	"  --token,                  -t  Token to authenticate with.\n"
	"  --version,                -v  Show versioning information.\n");
  exit (0);
}

void
show_version (void)
{
  puts ("sgfs " VERSION);
  exit (0);
}

static int
sgfs_getattr (const char *path, struct stat *st)
{
  scm_init_guile();

  SCM path_scm = scm_from_latin1_string (path);

  st->st_uid = getuid ();
  st->st_gid = getgid ();
  st->st_atime = time (NULL);
  st->st_mtime = time (NULL);

  if (scm_is_true (scm_call_1 (is_directory, path_scm)))
    {
      st->st_mode = S_IFDIR | 0500;
      st->st_nlink = 2;
    }
  else
    {
      st->st_mode = S_IFREG | 0400;
      st->st_nlink = 1;

      SCM attributes = scm_call_1 (attributes_for_path, path_scm);
      if (scm_is_true (scm_list_p (attributes)) &&
	  scm_length (attributes) > (scm_from_int (1)))
	{
	  SCM size_scm = scm_list_ref (attributes, scm_from_int (0));
	  st->st_size = scm_to_int (size_scm);
	  size_scm = NULL;
	}
      else
	st->st_size = 0;

      attributes = NULL;
    }

  path_scm = NULL;
  scm_gc ();

  return 0;
}

static int
sgfs_readdir (const char *path, void *buffer, fuse_fill_dir_t filldir,
              off_t offset, struct fuse_file_info *fi)
{
  scm_init_guile();

  SCM endpoint_scm = scm_from_latin1_string (endpoint);
  SCM token_scm    = scm_from_latin1_string (token);
  SCM path_scm     = scm_from_latin1_string (path);
  SCM paths        = scm_call_3 (directories_for_path,
                                 endpoint_scm,
                                 token_scm,
                                 path_scm);

  /* Add the usual suspects. */
  filldir (buffer, ".", NULL, 0);
  filldir (buffer, "..", NULL, 0);

  SCM elements = paths;
  while (scm_is_true (scm_pair_p (elements)))
    {
      SCM item = SCM_CAR (elements);

      char *filename = NULL;
      int size = 0;

      if (scm_is_true (scm_list_p (item)))
	{
	  SCM name_scm = scm_list_ref (item, scm_from_int (0));
	  SCM size_scm = scm_list_ref (item, scm_from_int (1));

	  filename = scm_to_locale_string (name_scm);
	  size = scm_to_int (size_scm);

	  struct stat st;
	  st.st_uid   = getuid();
	  st.st_gid   = getgid();
	  st.st_atime = time (NULL);
	  st.st_mtime = time (NULL);
	  st.st_mode  = S_IFREG | 0400;
	  st.st_nlink = 1;
	  st.st_size  = size;

	  filldir (buffer, filename, &st, 0);
	}
      else
	{
	  filename = scm_to_locale_string (item);
	  filldir (buffer, filename, NULL, 0);
	}

      free (filename);
      item = NULL;
      elements = SCM_CDR (elements);
    }

  elements = NULL;
  endpoint_scm = NULL;
  token_scm = NULL;
  path_scm = NULL;
  paths = NULL;

  scm_gc ();

  return 0;
}

static int
sgfs_read (const char *path, char *buffer, size_t size, off_t offset,
           struct fuse_file_info *fi)
{
  scm_init_guile();

  SCM path_scm     = scm_from_latin1_string (path);

  SCM attributes = scm_call_1 (attributes_for_path, path_scm);
  if (scm_is_true (scm_list_p (attributes)) &&
      scm_length (attributes) > (scm_from_int (1)))
    {
      SCM size_scm = scm_list_ref (attributes, scm_from_int (0));
      SCM content_scm = scm_list_ref (attributes, scm_from_int (2));

      char* content = scm_to_locale_string (content_scm);

      memcpy (buffer, content + offset, size);

      size_t content_len = strlen (content);

      free (content);
      path_scm = NULL;
      attributes = NULL;
      size_scm = NULL;
      content_scm = NULL;
      content = NULL;

      scm_gc();

      return content_len - offset;
    }
  else
    return -1;
}

static struct fuse_operations operations = {
    .getattr	= sgfs_getattr,
    .readdir	= sgfs_readdir,
    .read        = sgfs_read,
};

int
main (int argc, char *argv[])
{
  int arg          = 0;
  int index        = 0;
  char *error_log  = NULL;
  char *debug_log  = NULL;

  memset (token, '\0', 85);

  /* Program options
   * ------------------------------------------------------------------- */
  static struct option options[] =
    {
      { "debug-log",             required_argument, 0, 'd' },
      { "endpoint",              required_argument, 0, 'E' },
      { "error-log",             required_argument, 0, 'e' },
      { "help",                  no_argument,       0, 'h' },
      { "mountpoint",            required_argument, 0, 'm' },
      { "token",                 required_argument, 0, 't' },
      { "version",               no_argument,       0, 'v' },
      { 0,                       0,                 0, 0   }
    };

  while (arg != -1)
    {
      /* Make sure to list all short options in the string below. */
      arg = getopt_long (argc, argv, "d:E:e:hm:t:v", options, &index);
      switch (arg)
        {
        case 'd': debug_log  = optarg;                  break;
        case 'E': endpoint   = optarg;                  break;
        case 'e': error_log  = optarg;                  break;
        case 'h': show_help ();                         break;
        case 'm': mountpoint = optarg;                  break;
        case 't': strncpy (token, optarg, 84);          break;
        case 'v': show_version ();                      break;
        }
    }

  if (! mountpoint)
    {
      fprintf (stderr, "Please provide a --mountpoint.\n");
      exit (1);
    }

  if (! endpoint)
    {
      fprintf (stderr, "Please provide a --endpoint.\n");
      exit (1);
    }

  if (token[0] == '\0')
    {
      char *token_dyn = NULL;
      size_t token_len = 0;
      ssize_t token_read;

      printf ("Token: ");
      token_read = getline (&token_dyn, &token_len, stdin);
      if (token_read < 83 || token_read > 84)
        {
          fprintf (stderr, "This is an invalid token.\n");
          exit (1);
        }

      /* Copy the exact token length, so that we strip off the newline. */
      strncpy (token, token_dyn, 83);

      free (token_dyn);
      token_dyn = NULL;
    }
  else if (token[82] == '\0')
    {
      fprintf (stderr, "This is an invalid token.\n");
      exit (1);
    }

  /* At this point, we don't accept input from stdin anymore. */
  fclose (stdin);

  /* Make sure backtraces are wide enough to be useful. */
  setenv ("COLUMNS", "10000", 1);

  /* Avoid auto-compiling files. */
  setenv ("GUILE_AUTO_COMPILE", "0", 1);

  /* Allocate a small initial head size. */
  setenv ("GC_INITIAL_HEAP_SIZE", "20M", 1);

  char *guile_load_path = getenv ("GUILE_LOAD_PATH");
  char *guile_load_compiled_path = getenv ("GUILE_LOAD_COMPILED_PATH");

  /* Enhance load paths. */
  if (guile_load_path)
    {
      size_t total_len = strlen(guile_load_path) + strlen(GUILE_LOAD_PATH) + 2;
      char guile_load_path_full[total_len];

      snprintf (guile_load_path_full, total_len, "%s:%s",
                GUILE_LOAD_PATH, guile_load_path);

      setenv ("GUILE_LOAD_PATH", guile_load_path_full, 1);
    }
  else
    setenv ("GUILE_LOAD_PATH", GUILE_LOAD_PATH, 1);

  if (guile_load_compiled_path)
    {
      size_t total_len = strlen(guile_load_compiled_path) +
                         strlen(GUILE_LOAD_COMPILED_PATH) + 2;

      char guile_load_compiled_path_full[total_len];

      snprintf (guile_load_compiled_path_full, total_len, "%s:%s",
                GUILE_LOAD_COMPILED_PATH, guile_load_compiled_path);

      setenv ("GUILE_LOAD_COMPILED_PATH", guile_load_compiled_path_full, 1);
    }
  else
    setenv ("GUILE_LOAD_COMPILED_PATH", GUILE_LOAD_COMPILED_PATH, 1);

  /* Initialize Guile. */
  scm_init_guile ();

  log_debug = scm_c_public_ref ("logger", "log-debug");
  log_error = scm_c_public_ref ("logger", "log-error");

  SCM debug_port = NULL;
  SCM error_port = NULL;

  if (error_log)
    {
      SCM set_error_port = scm_c_public_ref ("logger", "set-default-error-port!");
      error_port = scm_open_file (scm_from_latin1_string (error_log),
                                  scm_from_latin1_string ("a"));
      scm_call (set_error_port, error_port, SCM_UNDEFINED);
      scm_call (log_error,
                scm_from_latin1_string ("main"),
                scm_from_latin1_string ("Started SGFS."),
                SCM_UNDEFINED);

      set_error_port = NULL;
    }

  if (debug_log)
    {
      SCM set_debug_port = scm_c_public_ref ("logger", "set-default-debug-port!");
      debug_port = scm_open_file (scm_from_latin1_string (debug_log),
                                  scm_from_latin1_string ("a"));
      scm_call (set_debug_port, debug_port, SCM_UNDEFINED);
      scm_call (log_debug,
                scm_from_latin1_string ("main"),
                scm_from_latin1_string ("Started SGFS."),
                SCM_UNDEFINED);

      set_debug_port = NULL;
    }

  debug_port = NULL;
  error_port = NULL;

  /* --------------------------------------------------------------------------
   * CREATE MUTEX
   * --------------------------------------------------------------------------
   */

  SCM set_projects_lock = scm_c_public_ref ("sgfs cache", "set-projects-lock!");
  SCM set_queries_lock  = scm_c_public_ref ("sgfs cache", "set-queries-lock!");

  scm_call_1 (set_projects_lock, scm_make_mutex());
  scm_call_1 (set_queries_lock,  scm_make_mutex());

  set_projects_lock = NULL;
  set_queries_lock  = NULL;

  scm_gc ();

  is_directory         = scm_c_public_ref ("sgfs filesystem", "is-directory");
  directories_for_path =
    scm_c_public_ref ("sgfs filesystem", "directory-overview-for-path");
  attributes_for_path =
    scm_c_public_ref ("sgfs filesystem", "attributes-for-path");

  /* We only pass a few arguments to ‘fuse_main’: “-f” to keep sgfs in the
   * foreground, and ‘mountpoint’ to indicate where to mount the virtual
   * filesystem. */
  char *fuse_argv[] = { argv[0], "-f", mountpoint };
  return fuse_main (3, (char **)fuse_argv, &operations, NULL);
}
