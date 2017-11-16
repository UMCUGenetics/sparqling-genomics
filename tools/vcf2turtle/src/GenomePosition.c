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

#include "GenomePosition.h"
#include "RuntimeConfiguration.h"
#include "helper.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern RuntimeConfiguration program_config;

char *
faldo_in_between_position_name (FaldoInBetweenPosition *range)
{
  if (range == NULL || program_config.reference == NULL)
    return NULL;

  if (range->name != NULL) return range->name;
  if (range->before == NULL || range->after == NULL ||
      range->before->chromosome == NULL || range->after->chromosome == NULL)
    return NULL;

  char *range_before_name = faldo_exact_position_name (range->before);
  char *range_after_name = faldo_exact_position_name (range->after);
  if (range_before_name == NULL || range_after_name == NULL)
    return NULL;

  range->name_len = 22 + strlen (range_before_name) +
                         strlen (range_after_name) +
                         strlen (program_config.reference);

  range->name = calloc (range->name_len + 1, sizeof (char));
  if (range->name == NULL)
    {
      range->name_len = 0;
      return NULL;
    }

  /* FIXME: In theory, the before->name and after->name could
   * be different.  We need to address this issue. */
  snprintf (range->name, range->name_len, "%s...%s",
            range_before_name,
            range_after_name);

  return range->name;
}

char *
faldo_exact_position_name (FaldoExactPosition *position)
{
  if (position == NULL || program_config.reference == NULL)
    return NULL;

  if (position->name != NULL)
    return position->name;

  if (position->chromosome == NULL)
    return NULL;

  if (position->position == 0)
    return NULL;

  position->name_len = 22 + position->chromosome_len +
                       strlen (program_config.reference);

  position->name = calloc (position->name_len + 1, sizeof (char));
  if (position->name == NULL)
    {
      position->name_len = 0;
      return NULL;
    }

  snprintf (position->name, position->name_len, "%s-%s-%u",
            program_config.reference,
            position->chromosome,
            position->position);

  return position->name;
}

char *
faldo_range_name (FaldoRange *range)
{
  if (range == NULL || range->start == NULL || range->end == NULL
      || program_config.reference == NULL)
    return NULL;

  if (range->name != NULL) return range->name;

  range->name_len = 22 + strlen (range->start->name) +
                         strlen (program_config.reference);

  range->name = calloc (range->name_len + 1, sizeof (char));
  if (range->name == NULL)
    {
      range->name_len = 0;
      return NULL;
    }

  /* FIXME: In theory, the start->name and end->name could
   * be different.  We need to address this theoretical issue. */
  snprintf (range->name, range->name_len, "%s:%s-%u-%u",
            program_config.reference,
            range->start->name,
            range->start->position,
            range->end->position);

  return range->name;
}

char *
faldo_position_name (FaldoBaseType *position)
{
  if (position == NULL || program_config.reference == NULL) return NULL;
  switch (position->_type)
    {
    case FALDO_IN_BETWEEN_POSITION:
      return faldo_in_between_position_name ((FaldoInBetweenPosition *)position);
    case FALDO_EXACT_POSITION:
      return faldo_exact_position_name ((FaldoExactPosition *)position);
    case FALDO_RANGE:
      return faldo_range_name ((FaldoRange *)position);
    case FALDO_UNKNOWN:
      return NULL;
    }
}

void
faldo_in_between_position_print (FaldoInBetweenPosition *range)
{
  if (range == NULL) return;

  char *range_name = faldo_in_between_position_name (range);
  if (range_name == NULL)
    fprintf (stderr, "faldo_in_between_position_print: Unexpected range == NULL.\n");

  char *before_name = faldo_exact_position_name (range->before);
  if (before_name == NULL)
    fprintf (stderr, "faldo_in_between_position_print: Unexpected before == NULL.\n");

  char *after_name = faldo_exact_position_name (range->after);
  if (after_name == NULL)
    fprintf (stderr, "faldo_in_between_position_print: Unexpected after == NULL.\n");

  printf ("ip:%s rdf:type faldo:InBetweenPosition ; faldo:before ep:%s ; "
          "faldo:after ep:%s .\n", range_name, before_name, after_name);
}

void
faldo_exact_position_print (FaldoExactPosition *position)
{
  if (position == NULL || program_config.reference == NULL) return;

  char *position_name = faldo_exact_position_name (position);
  if (position_name == NULL)
    fprintf (stderr, "faldo_exact_position_name: Unexpected position == NULL.\n");

  printf ("ep:%s rdf:type faldo:ExactPosition ; faldo:position %u ; "
          "faldo:reference %s:%s .\n",
          position_name, position->position,
          program_config.reference, position->chromosome);
}

void
faldo_range_print (FaldoRange *range)
{
  if (range == NULL) return;

  char *range_name = faldo_range_name (range);
  if (range_name == NULL)
    fprintf (stderr, "Unexpected range_name == NULL.\n");

  char *start_name = faldo_exact_position_name (range->start);
  if (start_name == NULL)
    fprintf (stderr, "Unexpected start_name == NULL.\n");

  char *end_name = faldo_exact_position_name (range->end);
  if (end_name == NULL)
    fprintf (stderr, "Unexpected end_name == NULL.\n");

  printf ("rp:%s rdf:type faldo:RangePosition ; faldo:begin ep:%s ; "
          "faldo:end ep:%s .\n", range_name, start_name, end_name);
}

void
faldo_position_print (FaldoBaseType *position)
{
  if (position == NULL) return;
  switch (position->_type)
    {
    case FALDO_IN_BETWEEN_POSITION:
      return faldo_in_between_position_print ((FaldoInBetweenPosition *)position);
    case FALDO_EXACT_POSITION:
      return faldo_exact_position_print ((FaldoExactPosition *)position);
    case FALDO_RANGE:
      return faldo_range_print ((FaldoRange *)position);
    case FALDO_UNKNOWN:
      return;
    }
}

void
faldo_in_between_position_initialize (FaldoInBetweenPosition *range)
{
  if (range == NULL) return;

  range->_type = FALDO_IN_BETWEEN_POSITION;
  range->name = NULL;
  range->name_len = 0;
  range->before = NULL;
  range->after = NULL;
}

void
faldo_exact_position_initialize (FaldoExactPosition *position)
{
  if (position == NULL) return;

  position->_type = FALDO_EXACT_POSITION;
  position->name = NULL;
  position->name_len = 0;
  position->chromosome = NULL;
  position->chromosome_len = 0;
  position->position = 0;
}

void
faldo_range_initialize (FaldoRange *range)
{
  if (range == NULL) return;

  range->_type = FALDO_RANGE;
  range->name = NULL;
  range->name_len = 0;
  range->start = NULL;
  range->end = NULL;
}

void
faldo_position_initialize (FaldoBaseType *position, FaldoType type)
{
  switch (type)
    {
    case FALDO_IN_BETWEEN_POSITION:
      faldo_in_between_position_initialize ((FaldoInBetweenPosition *)position);
      break;
    case FALDO_EXACT_POSITION:
      faldo_exact_position_initialize ((FaldoExactPosition *)position);
      break;
    case FALDO_RANGE:
      faldo_range_initialize ((FaldoRange *)position);
      break;
    default:
      position->_type = FALDO_UNKNOWN;
      break;
    }
}

void
faldo_in_between_position_reset (FaldoInBetweenPosition *range)
{
  if (range == NULL) return;
  if (range->name) free (range->name);
  faldo_in_between_position_initialize (range);
}

void
faldo_exact_position_reset (FaldoExactPosition *position)
{
  if (position == NULL) return;
  if (position->name != NULL) free (position->name);
  faldo_exact_position_initialize (position);
}

void
faldo_range_reset (FaldoRange *range)
{
  if (range == NULL) return;
  if (range->name != NULL) free (range->name);
  faldo_range_initialize (range);
}

void
faldo_position_reset (FaldoBaseType *position, FaldoType type)
{
  switch (type)
    {
    case FALDO_IN_BETWEEN_POSITION:
      faldo_in_between_position_reset ((FaldoInBetweenPosition *)position);
      break;
    case FALDO_EXACT_POSITION:
      faldo_exact_position_reset ((FaldoExactPosition *)position);
      break;
    case FALDO_RANGE:
      faldo_range_reset ((FaldoRange *)position);
      break;
    default:
      position->_type = FALDO_UNKNOWN;
      break;
    }
}

char*
faldo_position_prefix (FaldoBaseType *position)
{
  if (position == NULL || position->_type == FALDO_UNKNOWN)
    return "unknown";

  return (position->_type == FALDO_IN_BETWEEN_POSITION)
         ? "ip"
         : (position->_type == FALDO_EXACT_POSITION)
           ? "ep"
           : "rp";
}
