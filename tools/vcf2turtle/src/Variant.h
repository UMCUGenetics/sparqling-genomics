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

#include "GenomePosition.h"
#include <stdint.h>
#include <stdbool.h>
#include <htslib/vcf.h>

typedef enum {
  VARIANT,
  STRUCTURAL_VARIANT,
  SNP_VARIANT
} VariantType;

/*----------------------------------------------------------------------------.
 | VARIANT OBJECT STATE DESCRIPTION                                           |
 '----------------------------------------------------------------------------*/
typedef struct
{
  VariantType _obj_type;        /* For internal use only. */
  GenomePosition *position1;
  float quality;
  char *filter;
  char *type;
  uint32_t type_len;
  char *hash;

  /* These mirror the internal HTSLib record's information. */
  int filters_len;
  int *filters;
} Variant;


/*----------------------------------------------------------------------------.
 | STRUCTURALVARIANT OBJECT STATE DESCRIPTION                                 |
 '----------------------------------------------------------------------------*/
typedef struct
{
  /* Inherit the properties of a regular Variant.
   * ----------------------------------------------------------------------- */
  VariantType _obj_type;
  GenomePosition *position1;
  float quality;
  char *filter;
  char *type;
  uint32_t type_len;
  char *hash;

  /* These mirror the internal HTSLib record's information. */
  int filters_len;
  int *filters;

  /* Add properties specific to a StructuralVariant.
   * ----------------------------------------------------------------------- */
  GenomePosition *position2;

} StructuralVariant;

/* The SNPVariant has no additional properties in comparison to a Variant. */
typedef Variant SNPVariant;

/*
 * Even though these functions request a pointer to a Variant, we can also
 * pass a pointer to a StructuralVariant, because it has the same fields,
 * in the same order.
 *
 * The implementation of these functions will handle the type correctly, as
 * long as v->type has been set properly.
 */
char *hash_Variant (Variant *v, bcf_hdr_t *vcf_header, bool use_cache);
void print_Variant (Variant *v, bcf_hdr_t *vcf_header);

#endif  /* VARIANT_H */
