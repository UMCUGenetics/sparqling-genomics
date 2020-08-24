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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pdf_report.h"

HPDF_REAL ScaleDPI (HPDF_REAL size) { return size * (72.0F / 288.0F); }

int
report_init (report_t *report, char *filename)
{
  if (report == NULL) return 1;

  report->pdf        = HPDF_New (NULL, NULL);
  if (! report->pdf) return 1;

  /* Compress the PDF. */
  HPDF_SetCompressionMode (report->pdf, HPDF_COMP_ALL);
  HPDF_SetInfoAttr (report->pdf, HPDF_INFO_PRODUCER, "SPARQLing-genomics");

  const char* name   = HPDF_LoadTTFontFromFile (report->pdf, FONT_FILE, HPDF_TRUE);
  report->font       = HPDF_GetFont (report->pdf, name, NULL);
  if (! report->font) return 1;

  report->filename   = filename;
  report->padding    = 20;
  report->occupied_y = report->padding;

  /* Set A4 page size. */
  report->page = HPDF_AddPage (report->pdf);
  if (! report->page) return 1;

  HPDF_Page_SetSize (report->page, HPDF_PAGE_SIZE_A4, HPDF_PAGE_PORTRAIT);
  HPDF_Page_SetFontAndSize (report->page, report->font, 12);
  return 0;
}

void *
report_destroy (void *ptr)
{
  if (! ptr) return NULL;

  report_t *report = ptr;
  if (! report->pdf) return NULL;

  HPDF_Free (report->pdf);
  free (report->title);
  free (report->logo_filename);
  free (report);

  return NULL;
}

SCM
report_free (SCM data)
{
  report_t *report = scm_to_pointer (data);
  if (! report) return SCM_BOOL_F;

  report_destroy (report);
  return SCM_BOOL_T;
}

SCM
report_write (SCM data)
{
  report_t *report = scm_to_pointer (data);
  if (! report) return SCM_BOOL_F;
  if (! report->pdf) return SCM_BOOL_F;
  if (! report->filename) return SCM_BOOL_F;

  HPDF_SaveToFile (report->pdf, report->filename);

  return SCM_BOOL_T;
}

SCM
report_write_to_port (SCM port, SCM data, SCM chunked_writing_p)
{
  report_t *report = scm_to_pointer (data);
  if (! report) return SCM_BOOL_F;
  if (! report->pdf) return SCM_BOOL_F;

  if (scm_output_port_p (port) == SCM_BOOL_F)
    return SCM_BOOL_F;

  /* Set the default value for chunked_writing_p. */
  if (chunked_writing_p == SCM_UNDEFINED)
    chunked_writing_p = SCM_BOOL_F;

  /* Write the whole PDF to memory. */
  HPDF_SaveToStream (report->pdf);

  /* HPDF_BYTE is set to be an "unsigned char" in "hpdf_types.h". */
  HPDF_UINT32 buffer_length = 4096;
  HPDF_UINT32 read_bytes = buffer_length;
  HPDF_STATUS status = HPDF_OK;
  HPDF_BYTE buffer[buffer_length];

  const size_t size_buffer_length = 16;
  char size_buffer[size_buffer_length];
  int written = 0;

  /* The status will be HPDF_STREAM_EOF when all contents have
   * been sent. */
  while (status == HPDF_OK)
    {
      status = HPDF_ReadFromStream  (report->pdf, buffer, &read_bytes);

      if (read_bytes == 0) continue;
      if (scm_is_true (chunked_writing_p))
        {
          written = snprintf (size_buffer, size_buffer_length, "%x\r\n",
                              read_bytes);

          scm_c_write (port, size_buffer, written);
        }

      scm_c_write (port, buffer, read_bytes);
      scm_c_write (port, "\r\n", 2);

      /* The thing is that “read_bytes” is used both to indicate
       * the number of bytes to read upon calling HPDF_ReadFromStream,
       * as well as to communicate how many bytes were actually read
       * upon returning from HPDF_ReadFromStream.
       *
       * So to reuse the read_bytes variable, we have to reset it.
       */
      read_bytes = buffer_length;
    }

  /* So, since we can only check after sending the data out whether
   * an error occurred, we can't do any better than return #f here. */
  if (status != HPDF_STREAM_EOF)
    return SCM_BOOL_F;
  else if (scm_is_true (chunked_writing_p))
    scm_c_write (port, "0\r\n\r\n", 5);

  return SCM_BOOL_T;
}

SCM
report_pdf (SCM filename)
{
  report_t *report = malloc (sizeof (report_t));
  if (! report) return SCM_BOOL_F;

  memset (report, 0, sizeof (report_t));

  if (filename == SCM_UNDEFINED)
    report_init (report, NULL);
  else
    report_init (report, scm_to_locale_string (filename));

  return scm_from_pointer (report, NULL);
}

SCM
report_render_heading (SCM data, SCM title_scm, HPDF_REAL fontsize)
{
  report_t *report = scm_to_pointer (data);
  if (report == NULL) return SCM_BOOL_F;
  if (scm_string_p (title_scm) == SCM_BOOL_F) return SCM_BOOL_F;

  HPDF_REAL height = HPDF_Page_GetHeight (report->page);
  HPDF_REAL width = HPDF_Page_GetWidth (report->page);

  char *title = scm_to_locale_string (title_scm);

  HPDF_Page_BeginText (report->page);
  if (fontsize > 20)
    {
      HPDF_Page_SetLineWidth (report->page, 1);
      HPDF_Page_SetTextRenderingMode (report->page, HPDF_FILL_THEN_STROKE);
    }
  HPDF_Page_SetFontAndSize (report->page, report->font, fontsize);
  HPDF_REAL text_width = HPDF_Page_TextWidth (report->page, title);
  HPDF_Page_TextOut (report->page,
                     (width - text_width) / 2,
                     height - report->occupied_y - fontsize,
                     title);
  if (fontsize > 20)
    HPDF_Page_SetTextRenderingMode (report->page, HPDF_FILL);

  HPDF_Page_SetFontAndSize (report->page, report->font, 12);
  HPDF_Page_EndText (report->page);

  free (title);
  report->occupied_y += fontsize + report->padding / 2;
  return SCM_BOOL_T;
}

SCM
report_set_title (SCM data, SCM title_scm)
{
  report_t *report = scm_to_pointer (data);
  if (report == NULL) return SCM_BOOL_F;
  if (scm_string_p (title_scm) == SCM_BOOL_F) return SCM_BOOL_F;

  report->title = scm_to_locale_string (title_scm);
  HPDF_SetInfoAttr (report->pdf, HPDF_INFO_TITLE, report->title);

  return report_render_heading (data, title_scm, 24);
}

SCM
report_set_subtitle (SCM data, SCM title_scm)
{
  if (scm_string_p (title_scm) == SCM_BOOL_F) return SCM_BOOL_F;
  return report_render_heading (data, title_scm, 18);
}

SCM
report_set_logo (SCM data, SCM filename_scm, SCM position)
{
  report_t *report = scm_to_pointer (data);
  if (! report) return SCM_BOOL_F;
  if (scm_string_p (filename_scm) == SCM_BOOL_F) return SCM_BOOL_F;

  HPDF_REAL page_height  = HPDF_Page_GetHeight (report->page);
  HPDF_REAL page_width   = HPDF_Page_GetWidth (report->page);

  report->logo_filename  = scm_to_locale_string (filename_scm);
  HPDF_Image image       = HPDF_LoadPngImageFromFile (report->pdf,
                                                      report->logo_filename);
  HPDF_REAL image_width  = HPDF_Image_GetWidth (image);
  HPDF_REAL image_height = HPDF_Image_GetHeight (image);

  HPDF_REAL logo_padding = report->occupied_y;
  HPDF_REAL logo_width   = ScaleDPI (image_width);
  HPDF_REAL logo_height  = ScaleDPI (image_height);

  SCM center = scm_from_latin1_symbol ("center");
  SCM right  = scm_from_latin1_symbol ("right");

  HPDF_REAL logo_x_position;
  if (scm_is_true (scm_equal_p (position, center)))
    {
      logo_x_position = (page_width - logo_width) / 2;
      report->occupied_y += logo_height + (report->padding / 2);
    }
  else if (scm_is_true (scm_equal_p (position, right)))
    logo_x_position = page_width - logo_width - logo_padding;
  else
    logo_x_position = logo_padding;

  HPDF_Page_DrawImage (report->page, image,
                       logo_x_position,
                       page_height  - logo_height - logo_padding,
                       logo_width, logo_height);

  center = NULL;
  right = NULL;
  return SCM_BOOL_T;
}

SCM
report_render_text_field (SCM data, SCM label_scm, SCM text_scm, SCM lines_scm)
{
  report_t *report = scm_to_pointer (data);
  if (report == NULL) return SCM_BOOL_F;
  if (scm_string_p (text_scm) == SCM_BOOL_F) return SCM_BOOL_F;
  if (scm_string_p (label_scm) == SCM_BOOL_F) return SCM_BOOL_F;

  char *label = scm_to_locale_string (label_scm);
  char *text  = scm_to_locale_string (text_scm);

  int lines = 1;
  if (lines_scm != SCM_UNDEFINED)
    lines = scm_to_int (lines_scm);

  HPDF_REAL height       = HPDF_Page_GetHeight (report->page);
  HPDF_REAL width        = HPDF_Page_GetWidth (report->page);
  HPDF_REAL text_height  = 12;

  /* Display the field's label. */
  HPDF_Page_BeginText (report->page);
  HPDF_Page_TextOut (report->page,
                     report->padding,
                     height - report->occupied_y - report->padding,
                     label);
  HPDF_Page_EndText (report->page);

  /* Display a border around the field's text. */
  //HPDF_Page_SetLineWidth (report->page, 1);
  HPDF_Page_SetRGBFill (report->page, 0.97, 0.97, 0.97);
  HPDF_Page_Rectangle (report->page,
                       report->padding + 100 - 2,
                       /* The top-most base position. */
                       height - report->occupied_y -
                       report->padding - ((text_height + (text_height / 2)) * (lines - 1)) - 3,
                       width - 100 - report->padding * 2,
                       /* Text lines */
                       (text_height + 2) * lines -
                       /* Text spacing */
                       ((-1 * (text_height / 2) * (lines - 1))) + 2);
  HPDF_Page_Fill (report->page);
  HPDF_Page_SetRGBFill (report->page, 0, 0, 0);

  HPDF_Page_BeginText (report->page);
  HPDF_Page_SetRGBFill (report->page, 0.33, 0.33, 0.33);
  HPDF_Page_TextRect (report->page,
                      report->padding + 100,
                      height - report->occupied_y - 7,
                      width - report->padding,
                      height - report->occupied_y - report->padding * lines,
                      text,
                      HPDF_TALIGN_LEFT, NULL);
  HPDF_Page_SetRGBFill (report->page, 0, 0, 0);
  HPDF_Page_EndText (report->page);

  free (label);
  free (text);

  report->occupied_y += ((text_height + (text_height / 2) + 2) * lines);

  return SCM_BOOL_T;
}

SCM
report_render_section (SCM data, SCM text_scm)
{
  report_t *report = scm_to_pointer (data);
  if (report == NULL) return SCM_BOOL_F;
  if (scm_string_p (text_scm) == SCM_BOOL_F) return SCM_BOOL_F;

  HPDF_Page_SetRGBFill (report->page, 0.113, 0.345, 0.670);
  HPDF_Page_SetLineWidth (report->page, 1);
  HPDF_Page_SetRGBStroke (report->page, 0.113, 0.345, 0.670);
  report->occupied_y += report->padding / 2;
  SCM return_value = report_render_heading (data, text_scm, 18);
  HPDF_Page_SetRGBFill (report->page, 0, 0, 0);
  HPDF_Page_SetRGBStroke (report->page, 0, 0, 0);

  return return_value;
}

SCM
report_render_subsection (SCM data, SCM text_scm)
{
  report_t *report = scm_to_pointer (data);
  if (report == NULL) return SCM_BOOL_F;
  if (scm_string_p (text_scm) == SCM_BOOL_F) return SCM_BOOL_F;

  HPDF_REAL height = HPDF_Page_GetHeight (report->page);
  HPDF_REAL width = HPDF_Page_GetWidth (report->page);
  int fontsize = 16;

  char *title = scm_to_locale_string (text_scm);

  HPDF_Page_BeginText (report->page);
  HPDF_Page_SetFontAndSize (report->page, report->font, fontsize);
  HPDF_Page_TextOut (report->page,
                     report->padding,
                     height - report->occupied_y - fontsize,
                     title);
  HPDF_Page_SetFontAndSize (report->page, report->font, 12);
  HPDF_Page_EndText (report->page);

  /* Draw a line */
  HPDF_Page_SetLineWidth (report->page, 0.5);
  HPDF_Page_SetRGBStroke (report->page, 0.33, 0.33, 0.33);
  HPDF_Page_MoveTo (report->page,
                    report->padding,
                    height - report->occupied_y - 18);
  HPDF_Page_LineTo (report->page,
                    width - report->padding - 2,
                    height - report->occupied_y - 18);

  HPDF_Page_Stroke (report->page);
  HPDF_Page_SetRGBStroke (report->page, 0, 0, 0);

  free (title);
  report->occupied_y += fontsize + 2;
  return SCM_BOOL_T;
}

SCM
report_render_spacer (SCM data, SCM height_scm)
{
  report_t *report = scm_to_pointer (data);
  if (report == NULL) return SCM_BOOL_F;

  int height = scm_to_int (height_scm);
  report->occupied_y += height;

  return SCM_BOOL_T;
}

void
init_pdf_report ()
{
  scm_c_define_gsubr ("pdf-report",                    0, 1, 0, report_pdf);
  scm_c_define_gsubr ("pdf-report-set-title!",         2, 0, 0, report_set_title);
  scm_c_define_gsubr ("pdf-report-set-subtitle!",      2, 0, 0, report_set_subtitle);
  scm_c_define_gsubr ("pdf-report-set-logo!",          3, 0, 0, report_set_logo);
  scm_c_define_gsubr ("pdf-report-write!",             1, 0, 0, report_write);
  scm_c_define_gsubr ("pdf-report-write-to-port!",     2, 1, 0, report_write_to_port);
  scm_c_define_gsubr ("pdf-report-render-text-field!", 3, 1, 0, report_render_text_field);
  scm_c_define_gsubr ("pdf-report-render-spacer!",     2, 0, 0, report_render_spacer);
  scm_c_define_gsubr ("pdf-report-render-section!",    2, 0, 0, report_render_section);
  scm_c_define_gsubr ("pdf-report-render-subsection!", 2, 0, 0, report_render_subsection);
  scm_c_define_gsubr ("pdf-report-close",              1, 0, 0, report_free);
}
