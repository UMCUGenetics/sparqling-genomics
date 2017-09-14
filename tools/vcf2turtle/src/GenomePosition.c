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
#include <string.h>

extern RuntimeConfiguration program_config;


char *
faldo_in_between_position_name (FaldoInBetweenPosition *range)
{
  if (range == NULL || range->before == NULL || range->after == NULL)
    return NULL;

  if (range->name != NULL) return range->name;

  range->name_len = 22 + strlen (range->start->reference);
  range->name = calloc (range->name_len + 1, sizeof (char));
  if (range->name == NULL)
    {
      range->name_len = 0;
      return NULL;
    }

  /* FIXME: In theory, the before->reference and after->reference could
   * be different.  We need to address this issue. */
  snprintf (range->name, range->name_len, "%s:%u-%u",
            range->before->reference,
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

  position->name_len = 22 + strlen (position->reference);
  position->name = calloc (range->name_len + 1, sizeof (char));
  if (position->name == NULL)
    {
      position->name_len = 0;
      return NULL;
    }

  snprintf (range->name, range->name_len, "%s:%u-%u",
            range->start->reference,
            range->start->position,
            range->end->position);

  return range->name;
}

char *
faldo_range_name (FaldoRange *range)
{
  if (range == NULL || range->start == NULL || range->end == NULL)
    return NULL;

  if (range->name != NULL) return range->name;

  range->name_len = 22 + strlen (range->start->reference);
  range->name = calloc (range->name_len + 1, sizeof (char));
  if (range->name == NULL)
    {
      range->name_len = 0;
      return NULL;
    }

  /* FIXME: In theory, the start->reference and end->reference could
   * be different.  We need to address this theoretical issue. */
  snprintf (range->name, range->name_len, "%s:%u-%u",
            range->start->reference,
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
faldo_init_position (FaldoBaseType *position, FaldoBaseType type)
{
  if (position == NULL)
    return;

  position->_type = type;
  switch (type)
    {
    case FALDO_IN_BETWEEN_POSITION:
      {
        FaldoInBetweenPosition *p = ((FaldoInBetweenPosition *)position);
        p->name = NULL;
        p->name_len = 0;
        p->before = NULL;
        p->after = NULL;
      }
      break;
    case FALDO_EXACT_POSITION:
      {
        FaldoExactPosition *p = ((FaldoExactPosition *)position);
        p->name = NULL;
        p->name_len = 0;
        p->position = 0;
      }
      break;
    case FALDO_RANGE:
      {
        FaldoRange *p = ((FaldoRange *)position);
        p->name = NULL;
        p->name_len = 0;
        p->start = NULL;
        p->end = NULL;
      }
      break;
    case FALDO_UNKNOWN:
      return;
    }
}

void
faldo_in_between_position_print (FaldoInBetweenPosition *range)
{
  if (range == NULL) return;

}

void
faldo_exact_position_print (FaldoExactPosition *position)
{
  if (position == NULL) return;

  printf (buffer, 4096,
          "ep:%s rdf:type faldo:ExactPosition ; faldo:position %u ; "
          "faldo:reference %s .\n",
          faldo_exact_position_name ((FaldoExactPosition *)position),
          ((FaldoExactPosition *)position)->position
          ((FaldoExactPosition *)position)->reference);

}

void
faldo_range_print (FaldoRange *range)
{
  if (range == NULL) return;
}

void
faldo_position_print (FaldoBaseType *position)
{
  if (position == NULL) return;
}

/* void */
/* print_GenomePosition (GenomePosition *g) */
/* { */
/*   if (g == NULL) return; */

/*   if (g->cipos_len > 0) */
/*     { */
/*       printf ("  :confidence_interval_start %d ;\n", g->cipos[0]); */
/*       printf ("  :confidence_interval_end %d ;\n", g->cipos[1]); */
/*     } */
/* } */
