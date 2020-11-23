/*
 * Copyright © 2020  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include "r_report.h"
#include <Rinterface.h>

extern uintptr_t R_CStackLimit;
extern int R_SignalHandlers;

/* ----------------------------------------------------------------------------
 * INTERFACE TO R SWEAVE.
 * ----------------------------------------------------------------------------
 */

SCM
r_sweave_report (SCM rnw_filename_scm, SCM tex_filename_scm)
{
  /* Write a debug message.
   * ----------------------------------------------------------------------- */
  SCM log_debug  = scm_c_public_ref ("logger", "log-debug");
  SCM main_text  = scm_from_latin1_string ("r_sweave_report");
  SCM text       = scm_from_latin1_string ("Running embedded R.");

  scm_call_2 (log_debug, main_text, text);

  text      = NULL;
  main_text = NULL;
  log_debug = NULL;

  /* Deal with the filenames.
   * ----------------------------------------------------------------------- */
  SCM tex_dirname_scm = scm_dirname (tex_filename_scm);

  char *rnw_filename  = scm_to_locale_string (rnw_filename_scm);
  char *tex_filename  = scm_to_locale_string (tex_filename_scm);;
  char *tex_dirname   = scm_to_locale_string (tex_dirname_scm);
  char *pdf_filename  = strdup (tex_filename);

  tex_dirname_scm = NULL;

  if (! pdf_filename)
    {
      free (rnw_filename);
      free (tex_filename);
      free (tex_dirname);
      return SCM_BOOL_F;
    }

  size_t pdf_filename_len = strlen (pdf_filename);
  if (pdf_filename_len > 4)
    strcpy (pdf_filename + (pdf_filename_len - 4), ".pdf");
  else
    {
      free (rnw_filename);
      free (tex_filename);
      free (pdf_filename);
      return SCM_BOOL_F;
    }

  /* Run an embedded R.
   * ----------------------------------------------------------------------- */
  int r_error;
  char *r_argv[3] = { "R", "--no-save", "--no-echo" };
  setenv ("R_HOME", R_HOME, 0);

  R_CStackLimit = (uintptr_t)-1;
  R_SignalHandlers = 0;

  if (R_NilValue == NULL)
    Rf_initEmbeddedR (3, r_argv);

  SEXP r_cmd = NULL;
  PROTECT (r_cmd = lang2 (install ("setwd"),  mkString (tex_dirname)));
  R_tryEval (r_cmd, NULL, &r_error);

  /* Feel free to improve the way to make a function call with named
   * parameters.  */
  SEXP sweave_fn = PROTECT (Rf_allocVector (LANGSXP, 4));
  SETCAR (sweave_fn, Rf_install ("Sweave"));

  SEXP arg = CDR(sweave_fn);
  SETCAR (arg, mkString (rnw_filename));
  SET_TAG (arg, Rf_install ("file"));

  arg = CDR (arg);
  SETCAR (arg, Rf_ScalarLogical(TRUE));
  SET_TAG (arg, Rf_install ("quiet"));

  arg = CDR (arg);
  SETCAR (arg, mkString (tex_filename));
  SET_TAG (arg, Rf_install ("output"));

  R_tryEval (sweave_fn, NULL, &r_error);

  PROTECT (r_cmd = lang2 (install ("library"),  mkString ("tools")));
  R_tryEval (r_cmd, NULL, &r_error);

  SEXP texipdf_fn = PROTECT (Rf_allocVector (LANGSXP, 4));
  SETCAR (texipdf_fn, Rf_install ("texi2pdf"));

  arg = CDR(texipdf_fn);
  SETCAR (arg, mkString (tex_filename));
  SET_TAG (arg, Rf_install ("file"));

  arg = CDR(arg);
  SETCAR (arg, Rf_ScalarLogical(TRUE));
  SET_TAG (arg, Rf_install ("clean"));

  arg = CDR(arg);
  SETCAR (arg, Rf_ScalarLogical(TRUE));
  SET_TAG (arg, Rf_install ("quiet"));

  R_tryEval (texipdf_fn, NULL, &r_error);

  UNPROTECT (4);

  Rf_endEmbeddedR (0);

  /* Clean up.
   * ---------------------------------------------------------------------- */
  free (rnw_filename);
  rnw_filename = NULL;
  free (tex_filename);
  tex_filename = NULL;

  return SCM_BOOL_T;
}

void
init_r_report ()
{
  scm_c_define_gsubr ("r-sweave-report", 2, 0, 0, r_sweave_report);
}
