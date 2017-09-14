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

#ifndef UI_H
#define UI_H

#include <stdint.h>

/*----------------------------------------------------------------------------.
 | GENERAL UI STUFF                                                           |
 '----------------------------------------------------------------------------*/

void show_help (void);
void show_version (void);

/*----------------------------------------------------------------------------.
 | ERROR HANDLING                                                             |
 '----------------------------------------------------------------------------*/

int32_t print_vcf_file_error (const char *file_name);
int32_t print_vcf_header_error (const char *file_name);
int32_t print_memory_error (const char *file_name);
int32_t print_file_format_error (void);

#endif /* UI_H */
