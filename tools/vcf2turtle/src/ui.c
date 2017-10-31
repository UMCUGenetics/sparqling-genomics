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

#include "ui.h"
#include <stdio.h>
#include <stdlib.h>

void
show_help (void)
{
  puts ("\nAvailable options:\n"
        "  --input-file=ARG,   -i  The input file to process.\n"
        "  --filter=ARG,       -f  Omit calls with FILTER=ARG from the "
                                   "output.\n"
        "  --keep=ARG,         -k  Omit calls without FILTER=ARG from the "
                                   "output.\n"
        "  --graph-location,   -g  Location of the graph.\n"
        "  --reference-genome  -r  The reference genome the variant positions "
                                  "refer to.\n"
        "                          Valid values are: 'GRCh37', GRCh38'.\n"
        "  --caller            -c  The caller used to produce the VCF file.\n"
	"  --threads,          -t  Number of threads to use.\n"
	"  --version,          -v  Show versioning information.\n"
	"  --help,             -h  Show this message.\n");
}

void
show_version (void)
{
  puts ("Version: 0.0.1\n");
}

int32_t
print_vcf_file_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot open '%s'.\n", file_name);
  return 1;
}

int32_t
print_vcf_header_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Cannot read header of '%s'.\n", file_name);
  return 1;
}

int32_t
print_memory_error (const char *file_name)
{
  fprintf (stderr, "ERROR: Not enough memory available for processing '%s'.\n", file_name);
  return 1;
}

int32_t
print_file_format_error (void)
{
  fprintf (stderr, "ERROR: This program only handles \".vcf\" and \".vcf.gz\" files.\n");
  return 1;
}
