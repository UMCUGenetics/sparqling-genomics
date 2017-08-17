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
#include <string.h>
#include "helper.h"

int
main (int argc, char **argv)
{
  unsigned char data[] = {
    0x32, 0x56, 0x68, 0x12, 0x43, 0x59, 0x89, 0x97, 0x88, 0x11,
    0x76, 0x34, 0x22, 0x93, 0x66, 0x37, 0x62, 0x68, 0x73, 0x10,
    0x23, 0x26, 0x29, 0x61, 0x78, 0x87, 0x55, 0x19, 0x29, 0x27,
    0x25, 0x06
  };

  char obtained[65];
  const char *expected = "3256681243598997881176342293663762687310232629617887"
                         "551929272506";

  if (get_pretty_hash (data, 32, obtained))
    {
      if (strcmp (obtained, expected) != 0)
        {
          fputs ("get_pretty_hash ....................... FAIL\n", stderr);
          fprintf (stderr, "  The expected and obtained hashes differ.\n"
                           "  Expected: '%s'\n"
                           "  Obtained: '%s'\n", expected, obtained);
          return 1;
        }
      else
        {
          puts ("get_pretty_hash ....................... OK");
          return 0;
        }
    }
  else
    {
      fputs ("get_pretty_hash ....................... FAIL\n", stderr);
      return 1;
    }

  return 0;
}
