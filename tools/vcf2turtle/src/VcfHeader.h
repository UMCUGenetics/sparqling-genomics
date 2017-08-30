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

#ifndef VCFHEADER_H
#define VCFHEADER_H

#include "Origin.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef enum {
  HEADER_TYPE_UNKNOWN,
  HEADER_TYPE_GENERIC,
  HEADER_TYPE_INFO,
  HEADER_TYPE_FILTER,
  HEADER_TYPE_FORMAT,
  HEADER_TYPE_CONTIG
} HeaderType;

typedef struct
{
  HeaderType _type;
  char hash[65];
  Origin *origin;

  char *key;
  int32_t key_len;
  char *value;
  int32_t value_len;
} VcfHeader;

typedef struct
{
  HeaderType _type;
  char hash[65];
  Origin *origin;

  char *id;
  int32_t id_len;
  char *number;
  int32_t number_len;
  char *type;
  int32_t type_len;
  char *description;
  int32_t description_len;
} VcfInfoField;

typedef struct
{
  HeaderType _type;
  char hash[65];
  Origin *origin;

  char *id;
  int32_t id_len;
  char *description;
  int32_t description_len;
} VcfFilterField;

typedef struct
{
  HeaderType _type;
  char hash[65];
  Origin *origin;

  char *id;
  int32_t id_len;
  int32_t length;
  char *assembly;
  int32_t assembly_len;
} VcfContigField;

/* A INFO field contains the same elements as a FORMAT field. */
#define VcfFormatField VcfInfoField

char *hash_VcfHeader (VcfHeader *g, bool use_cache);
void print_VcfHeader (VcfHeader *g);
void initialize_VcfHeader (VcfHeader *h, HeaderType type);
void reset_VcfHeader (VcfHeader *g);

#endif  /* VCFHEADER_H */
