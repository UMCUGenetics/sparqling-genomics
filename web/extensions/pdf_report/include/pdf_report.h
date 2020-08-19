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

#include <libguile.h>
#include "hpdf.h"

#define FONT_FILE   "/home/cog/rjanssen2/sources/sparqling-genomics/web/static/fonts/Roboto-Light.ttf"
#define SG_LOGO     "/home/cog/rjanssen2/sources/sparqling-genomics/web/static/images/logo.png"

typedef struct
{
  HPDF_Doc  pdf;
  HPDF_Page page;
  HPDF_Font font;
  HPDF_REAL occupied_y;
  HPDF_REAL padding;

  const char *font_name;
  char *filename;
  char *logo_filename;
  char *title;

} report_t;

int report_init (report_t *report, char *filename);
SCM report_write (SCM report);
void *report_destroy (void *ptr);
SCM report_pdf (SCM filename);
SCM report_set_title (SCM data, SCM title_scm);
SCM report_set_logo (SCM data, SCM filename_scm, SCM position);
void init_pdf_report ();
