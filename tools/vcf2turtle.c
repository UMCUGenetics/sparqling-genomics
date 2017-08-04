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
#include <htslib/vcf.h>

static int32_t
print_usage (const char *program_name, int32_t return_value)
{
  printf ("Usage:\n  %s file.vcf\n", program_name);
  return return_value;
}

static int32_t
print_vcf_file_error (const char *filename)
{
  printf ("Cannot open '~s'\n", filename);
  return 1;
}

int32_t
main (int argc, char **argv)
{
  if (argc < 2) print_usage (argv[0], 0);
  if (argv[1] == "--help") print_usage (argv[0], 0);

  bcf1_t *buffer = NULL;
  bcf_hdr_t *vcf_header = NULL;
  htsFile *vcf_stream = NULL;

  /*--------------------------------------------------------------------------.
   | OPEN THE VCF/BCF FILE                                                    |
   '--------------------------------------------------------------------------*/

  /* Check whether the file is compressed. */
  int32_t filename_len = strlen (argv[1]);
  if (!strcmp ((argv[1] + filename_len - 3), ".gz"))
    vcf_stream = hts_open (argv[1], "rz");
  else
    vcf_stream = hts_open (argv[1], "r");

  /* Make sure the stream is OK. */
  if (vcf_stream == NULL)
    return print_vcf_file_error (argv[1]);

  /* Read the VCF header. */
  vcf_header = bcf_hdr_read (vcf_stream);
  if (vcf_header == NULL)
    {
      hts_close (vcf_stream);
      return print_vcf_file_error (argv[1]);
    }

  puts ("@prefix : <http://localhost:8890/TestGraph/> .");
  puts ("@prefix v: <http://localhost:8890/TestGraph/Variant/> .");
  puts ("@prefix p: <http://localhost:8890/TestGraph/Position/> .");
  puts ("@prefix s: <http://localhost:8890/TestGraph/Sample/> .");
  puts ("");

  /*--------------------------------------------------------------------------.
   | TRANSFORM SAMPLE NAMES IN VCF.                                           |
   '--------------------------------------------------------------------------*/

  int32_t i = 0;
  int32_t samples_num = bcf_hdr_nsamples (vcf_header);
  uint32_t sample_counter = 0;
  for (; i < samples_num; i++)
    {
      printf ("s:s%u a :Sample ;\n", sample_counter);
      printf ("  :name \"%s\" .\n", vcf_header->samples[i]);
      sample_counter++;
    }

  /*--------------------------------------------------------------------------.
   | TRANSFORM VARIANT CALL RECORDS.                                          |
   '--------------------------------------------------------------------------*/

  uint32_t variant_counter = 0;
  uint32_t position_counter = 0;

  buffer = bcf_init ();
  while (bcf_read (vcf_stream, vcf_header, buffer) == 0)
    {
      uint32_t head_position_counter = position_counter;
      uint32_t tail_position_counter = position_counter + 1;

      printf ("p:p%08u a :GenomePosition ;\n", head_position_counter);
      printf ("  :position   %d ;\n", buffer->pos);
      printf ("  :chromosome \"%s\" .\n\n", bcf_seqname (vcf_header, buffer));

      bcf_info_t *end_info = bcf_get_info(vcf_header, buffer, "END");
      bcf_info_t *chr2_info = bcf_get_info(vcf_header, buffer, "CHR2");

      if (end_info == NULL)
        puts ("# Missing END in the INFO field.");
      else
        {
          printf ("p:p%08u a :GenomePosition ;\n", tail_position_counter);
          printf ("  :position   %d ;\n", end_info->v1.i);

          if (chr2_info != NULL)
            {
              int32_t chr2_len = chr2_info->len;
              char chr2[chr2_info->len + 1];
              memset (chr2, '\0', chr2_info->len + 1);
              memcpy (chr2, chr2_info->vptr, chr2_info->len);
              printf ("  :chromosome \"%s\" .\n\n", chr2);
            }
          else
            /* Use the same chromosome as indicated by the CHROM field. */
            printf ("  :chromosome \"%s\" .\n\n", bcf_seqname (vcf_header, buffer));
        }

      printf ("v:v%08u a :Variant ;\n", variant_counter);
      printf ("  :head_position  p:p%08u ;\n", head_position_counter);
      printf ("  :tail_position  p:p%08u ;\n", tail_position_counter);
      printf ("  :quality        %f .\n\n", buffer->qual);

      variant_counter++;
      position_counter += 2;

      bcf_clear (buffer);
    }

  bcf_destroy (buffer);
  bcf_hdr_destroy (vcf_header);
  hts_close (vcf_stream);

  return 0;
}
