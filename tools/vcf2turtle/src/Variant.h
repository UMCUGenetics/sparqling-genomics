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

#ifndef VARIANT_H
#define VARIANT_H

#include "Origin.h"
#include "GenomePosition.h"
#include "VcfHeader.h"
#include <stdint.h>
#include <stdbool.h>
#include <htslib/vcf.h>

/* Reflect the types specified by htslib.
 * Please do not hardcode these values. */
typedef enum {
  VARIANT_TYPE_UNKNOWN = 99,
  VARIANT_TYPE_REF     = VCF_REF,
  VARIANT_TYPE_SNP     = VCF_SNP,
  VARIANT_TYPE_MNP     = VCF_MNP,
  VARIANT_TYPE_INDEL   = VCF_INDEL,
  VARIANT_TYPE_OTHER   = VCF_OTHER,
  VARIANT_TYPE_BND     = VCF_BND
} VariantType;

typedef enum {
  DATA_TYPE_INT,
  DATA_TYPE_FLOAT,
  DATA_TYPE_STRING
} DataType;

typedef struct {
  VcfInfoField *info_field;
  void *value;
  DataType value_type;
  int32_t value_len;            /* In case of the string type. */
} VariantInfoField;

typedef struct {
  VcfFormatField *format_field;
  void *value;
  DataType value_type;
  int32_t value_len;
} VariantFormatField;

/*----------------------------------------------------------------------------.
 | VARIANT OBJECT STATE DESCRIPTION                                           |
 '----------------------------------------------------------------------------*/
typedef struct
{
  VariantType _obj_type;        /* For internal use only. */
  Origin *origin;
  FaldoBaseType *position;
  float quality;
  char *reference;
  char *alternative;
  char *filter;
  unsigned char *type;
  uint32_t type_len;
  char name[65];

  /* These mirror the internal HTSLib record's information. */
  int filters_len;
  int *filters;
} Variant;

char *variant_name (Variant *v, bcf_hdr_t *vcf_header);
void variant_print (Variant *v, bcf_hdr_t *vcf_header);
void variant_initialize (Variant *v, VariantType type);
void variant_reset (Variant *v);

#endif  /* VARIANT_H */
