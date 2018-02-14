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

#include "runtime_configuration.h"
#include "ui.h"
#include "helper.h"
#include <stdio.h>

bool
runtime_configuration_init (void)
{
  config.filter = NULL;
  config.keep = NULL;
  config.input_file = NULL;
  config.reference = NULL;
  config.graph_location = NULL;
  config.caller = NULL;
  config.threads = 1;
  config.jobs_per_thread = 50000;

  return true;
}

bool
runtime_configuration_redland_init (void)
{
  /* Build an in-memory RDF store
   * --------------------------------------------------------------------
   *
   * The general idea is that a 'model' represent a graph, which can
   * be persisted on a 'storage'.  The hierarchy is: 
   * world <- storage <- model.
   * 
   * For performance, we use an in-memory model.  We could look into
   * directly connecting it to an existing triple store at a later time.
   * 
   * The serializer is used to output RDF in a certain format.  By default,
   * we use the Turtle format because it's the most dense format. */
  
  config.rdf_world = librdf_new_world ();
  if (!config.rdf_world)
    return (ui_print_redland_error () == 0);

  config.rdf_storage = librdf_new_storage (config.rdf_world, "memory", NULL, NULL);
  if (!config.rdf_storage)
    return (ui_print_redland_error () == 0);

  config.rdf_model = librdf_new_model (config.rdf_world, config.rdf_storage, NULL);
  if (!config.rdf_model)
    return (ui_print_redland_error () == 0);

  config.uris[URI_GRAPH_LOCATION]     = new_uri (config.graph_location);
  config.uris[URI_RDF]                = new_uri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
  config.uris[URI_RDFS]               = new_uri ("http://www.w3.org/2000/01/rdf-schema#");
  config.uris[URI_XSD]                = new_uri ("http://www.w3.org/2001/XMLSchema#");
  config.uris[URI_FALDO]              = new_uri ("http://biohackathon.org/resource/faldo#");
  config.uris[URI_VCF]                = new_uri ("http://ontologies.op.umcutrecht.nl/");
  config.uris[URI_VCF_ORIGIN]         = new_uri ("http://ontologies.op.umcutrecht.nl/Origin");
  config.uris[URI_VCF_HEADER]         = new_uri ("http://ontologies.op.umcutrecht.nl/VcfHeaderItem");
  config.uris[URI_VCF_HEADER_GENERIC] = new_uri ("http://ontologies.op.umcutrecht.nl/VcfHeaderGenericItem");
  config.uris[URI_VCF_HEADER_INFO]    = new_uri ("http://ontologies.op.umcutrecht.nl/VcfHeaderInfoItem");
  config.uris[URI_VCF_HEADER_FORMAT]  = new_uri ("http://ontologies.op.umcutrecht.nl/VcfHeaderFormatItem");
  config.uris[URI_VCF_HEADER_FILTER]  = new_uri ("http://ontologies.op.umcutrecht.nl/VcfHeaderFilterItem");
  config.uris[URI_VCF_HEADER_ALT]     = new_uri ("http://ontologies.op.umcutrecht.nl/VcfHeaderAltItem");
  config.uris[URI_VCF_HEADER_CONTIG]  = new_uri ("http://ontologies.op.umcutrecht.nl/VcfHeaderContigItem");
  config.uris[URI_VCF_SAMPLE]         = new_uri ("http://ontologies.op.umcutrecht.nl/Sample");
  config.uris[URI_VCF_VARIANT]        = new_uri ("http://ontologies.op.umcutrecht.nl/Variant");

  /* This is not ideal, so keep the number of URIs defined above in sync
   * with the number of URIs below. */
  int32_t uri_index = 0;
  for (; uri_index < URI_VCF_VARIANT; uri_index++)
    if (! config.uris[uri_index]) break;

  if (uri_index < URI_VCF_VARIANT)
    return (ui_print_redland_error () == 0);

  config.types[TYPE_STRING]  = new_uri ("http://www.w3.org/2001/XMLSchema#string");
  config.types[TYPE_INTEGER] = new_uri ("http://www.w3.org/2001/XMLSchema#integer");
  config.types[TYPE_FLOAT]   = new_uri ("http://www.w3.org/2001/XMLSchema#float");
  if (! (config.types[TYPE_STRING]
         && config.types[TYPE_INTEGER]
         && config.types[TYPE_FLOAT]))
    return (ui_print_redland_error () == 0);

  config.types[TYPE_STRING] = new_uri ("http://www.w3.org/2001/XMLSchema#string");

  config.rdf_serializer = librdf_new_serializer (config.rdf_world,
                                                 NULL, "text/turtle", NULL);

  if (!config.rdf_serializer)
    return (ui_print_redland_error () == 0);

  return true;
}

void
runtime_configuration_free (void)
{
  /* Free the memory of the URIs. */
  int32_t index;
  for (index = 0; index < NUMBER_OF_URIS; index++)
    librdf_free_uri (config.uris[index]);

  /* Free the memory of the TYPES. */
  for (index = 0; index < NUMBER_OF_TYPES; index++)
    librdf_free_uri (config.types[index]);

  /* Free the memory of the RDF world. */
  librdf_free_serializer (config.rdf_serializer);
  librdf_free_storage (config.rdf_storage);
  librdf_free_model (config.rdf_model);
  librdf_free_world (config.rdf_world);
}
