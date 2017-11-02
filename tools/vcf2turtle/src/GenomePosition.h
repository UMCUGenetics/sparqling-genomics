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

#ifndef GENOMEPOSITION_H
#define GENOMEPOSITION_H

#include <stddef.h>
#include <stdint.h>

typedef enum
{
  DIRECTION_UNKNOWN,
  DIRECTION_3_TO_5,
  DIRECTION_5_TO_3
} DirectionType;

typedef enum
{
  FALDO_UNKNOWN,
  FALDO_IN_BETWEEN_POSITION,
  FALDO_EXACT_POSITION,
  FALDO_RANGE
} FaldoType;

typedef struct
{
  FaldoType _type;
  char *name;
  size_t name_len;

} FaldoBaseType;

typedef struct
{
  FaldoType _type;
  char *name;
  size_t name_len;

  uint32_t position;
  char *chromosome;
  size_t chromosome_len;

} FaldoExactPosition;

typedef struct
{
  FaldoType _type;
  char *name;
  size_t name_len;

  FaldoExactPosition *before;
  FaldoExactPosition *after;

} FaldoInBetweenPosition;

typedef struct
{
  FaldoType _type;
  char *name;
  size_t name_len;

  FaldoExactPosition *start;
  FaldoExactPosition *end;
} FaldoRange;

/* Initialization functions. */
void faldo_in_between_position_initialize (FaldoInBetweenPosition *range);
void faldo_exact_position_initialize (FaldoExactPosition *position);
void faldo_range_initialize (FaldoRange *range);
void faldo_position_initialize (FaldoBaseType *position, FaldoType type);

/* Destructor functions. */
void faldo_in_between_position_reset (FaldoInBetweenPosition *range);
void faldo_exact_position_reset (FaldoExactPosition *position);
void faldo_range_reset (FaldoRange *range);
void faldo_position_reset (FaldoBaseType *position, FaldoType type);

/* Identifier generators. */
char *faldo_in_between_position_name (FaldoInBetweenPosition *range);
char *faldo_exact_position_name (FaldoExactPosition *position);
char *faldo_range_name (FaldoRange *range);
char *faldo_position_name (FaldoBaseType *position);

/* Display functions */
void faldo_in_between_position_print (FaldoInBetweenPosition *range);
void faldo_exact_position_print (FaldoExactPosition *position);
void faldo_range_print (FaldoRange *range);
void faldo_position_print (FaldoBaseType *position);

#endif  /* GENOMEPOSITION_H */
