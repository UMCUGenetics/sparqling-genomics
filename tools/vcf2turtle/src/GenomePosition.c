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
  if (range == NULL || range->before == NULL || range->after == NULL)
    return NULL;

  if (range->name != NULL) return range->name;

  range->name_len = 22 + strlen (range->before->name);
  range->name = calloc (range->name_len + 1, sizeof (char));
  if (range->name == NULL)
    {
      range->name_len = 0;
      return NULL;
    }

  /* FIXME: In theory, the before->name and after->name could
   * be different.  We need to address this issue. */
  snprintf (range->name, range->name_len, "%s:%u-%u",
            range->before->name,
            range->before->position,
            range->after->position);

  return range->name;
}

char *
faldo_exact_position_name (FaldoExactPosition *position)
{
  if (position == NULL)
    return NULL;

  if (position->name != NULL) return position->name;

  position->name_len = 22 + position->chromosome_len;
  position->name = calloc (position->name_len + 1, sizeof (char));
  if (position->name == NULL)
    {
      position->name_len = 0;
      return NULL;
    }

  snprintf (position->name, position->name_len, "%s:%u",
            position->chromosome,
            position->position);

  return position->name;
}

char *
faldo_range_name (FaldoRange *range)
{
  if (range == NULL || range->start == NULL || range->end == NULL)
    return NULL;

  if (range->name != NULL) return range->name;

  range->name_len = 22 + strlen (range->start->name);
  range->name = calloc (range->name_len + 1, sizeof (char));
  if (range->name == NULL)
    {
      range->name_len = 0;
      return NULL;
    }

  /* FIXME: In theory, the start->name and end->name could
   * be different.  We need to address this theoretical issue. */
  snprintf (range->name, range->name_len, "%s:%u-%u",
            range->start->name,
            range->start->position,
            range->end->position);

  return range->name;
}

char *
faldo_position_name (FaldoBaseType *position)
{
  if (position == NULL) return NULL;
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

  FaldoInBetweenPosition *p = (FaldoInBetweenPosition *)range;

  printf ("ip:%s rdf:type faldo:InBetweenPosition ; faldo:before ep:%s ; "
          "faldo:after ep:%s .\n",
          faldo_in_between_position_name (p),
          faldo_exact_position_name (p->before),
          faldo_exact_position_name (p->after));
}

void
faldo_exact_position_print (FaldoExactPosition *position)
{
  if (position == NULL) return;

  FaldoExactPosition *p = (FaldoExactPosition *)position;

  printf ("ep:%s rdf:type faldo:ExactPosition ; faldo:position %u ; "
          "faldo:reference %s:%s .\n",
          faldo_exact_position_name (p), p->position,
          program_config.reference, p->name, p->chromosome);
}

void
faldo_range_print (FaldoRange *range)
{
  if (range == NULL) return;

  FaldoRange *p = (FaldoRange *)range;

  printf ("ip:%s rdf:type faldo:InBetweenPosition ; faldo:begin ep:%s ; "
          "faldo:end ep:%s .\n",
          faldo_range_name (p),
          faldo_exact_position_name (p->start),
          faldo_exact_position_name (p->end));
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
  if (position->name) free (position->name);
  faldo_exact_position_initialize (position);
}

void
faldo_range_reset (FaldoRange *range)
{
  if (range == NULL) return;
  if (range->name) free (range->name);
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
