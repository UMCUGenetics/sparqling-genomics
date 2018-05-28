/*
 * Copyright (C) 2018  Roel Janssen <roel@gnu.org>
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

#ifndef HELPER_H
#define HELPER_H

#include <stdbool.h>
#include "runtime_configuration.h"

bool get_pretty_hash (unsigned char *hash,
                      uint32_t length,
                      unsigned char *output);

unsigned char *helper_get_hash_from_file (const char *filename);

/*----------------------------------------------------------------------------.
 | CONVENIENCE MACROS                                                         |
 | -------------------------------------------------------------------------- |
 | These macros introduce a level of Lisp-like magic based on the idea that   |
 | the variable names are consistent throughout the program.  We enforced     |
 | that by making a global state object called 'config'.                      |
 |                                                                            |
 |                   “I love it when a plan comes together.”                  |
 '----------------------------------------------------------------------------*/

#define new_node(uri, value)                                                \
  librdf_new_node_from_uri_local_name (config.rdf_world, uri,               \
                                       (unsigned char *)value)

#define new_node_from_uri(index)                                            \
  librdf_new_node_from_uri (config.rdf_world, index)

#define copy(node) librdf_new_node_from_node (node)
#define new_uri(uri) librdf_new_uri (config.rdf_world, (unsigned char*)uri)
#define rdf_serialize(model)                                                \
  librdf_serializer_serialize_model_to_file_handle                          \
  (config.rdf_serializer, stdout, config.uris[URI_ONTOLOGY_PREFIX], model)

#define add_triplet(s, p, o) librdf_model_add (config.rdf_model, s, p, o)
#define add_literal(s, p, value, data_type)                                 \
  librdf_model_add_typed_literal_statement                                  \
  (config.rdf_model, s, p, (unsigned char*)value, NULL, data_type)

#endif /* HELPER_H */
