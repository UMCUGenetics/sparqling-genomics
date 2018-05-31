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
#include <stdlib.h>

bool
runtime_configuration_init (void)
{
  config.filter = NULL;
  config.keep = NULL;
  config.input_file = NULL;
  config.reference = NULL;
  config.caller = NULL;
  //config.threads = 1;
  //config.jobs_per_thread = 50000;
  config.non_unique_variant_counter = 0;
  config.info_field_indexes = NULL;
  config.info_field_indexes_len = 0;
  config.show_progress_info = false;

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

  config.rdf_storage = librdf_new_storage (config.rdf_world, "hashes", NULL, "hash-type='memory'");

  if (!config.rdf_storage)
    return (ui_print_redland_error () == 0);

  config.rdf_model = librdf_new_model (config.rdf_world, config.rdf_storage, NULL);
  if (!config.rdf_model)
    return (ui_print_redland_error () == 0);

  config.uris[URI_ONTOLOGY_PREFIX]           = new_uri (ONTOLOGY_URI);
  config.uris[URI_RDF_PREFIX]                = new_uri ("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
  config.uris[URI_RDFS_PREFIX]               = new_uri ("http://www.w3.org/2000/01/rdf-schema#");
  config.uris[URI_XSD_PREFIX]                = new_uri ("http://www.w3.org/2001/XMLSchema#");
  config.uris[URI_FALDO_PREFIX]              = new_uri ("http://biohackathon.org/resource/faldo#");
  config.uris[URI_HG19_PREFIX]               = new_uri ("http://rdf.biosemantics.org/data/genomeassemblies/hg19#");
  config.uris[URI_HG19_CHR_PREFIX]           = new_uri ("http://rdf.biosemantics.org/data/genomeassemblies/hg19#chr");
  config.uris[URI_VCF_HEADER_GENERIC_PREFIX] = new_uri (ONTOLOGY_URI "VcfHeaderGenericItem/");
  config.uris[URI_VCF_HEADER_INFO_PREFIX]    = new_uri (ONTOLOGY_URI "VcfHeaderInfoItem/");
  config.uris[URI_VCF_HEADER_FORMAT_PREFIX]  = new_uri (ONTOLOGY_URI "VcfHeaderFormatItem/");
  config.uris[URI_VCF_HEADER_FILTER_PREFIX]  = new_uri (ONTOLOGY_URI "VcfHeaderFilterItem/");
  config.uris[URI_VCF_HEADER_ALT_PREFIX]     = new_uri (ONTOLOGY_URI "VcfHeaderAltItem/");
  config.uris[URI_VCF_HEADER_CONTIG_PREFIX]  = new_uri (ONTOLOGY_URI "VcfHeaderContigItem/");
  config.uris[URI_VCF_VC_PREFIX]             = new_uri (ONTOLOGY_URI "VariantCall/");

  /* NOTE: Keep the number of URIs defined above in sync with the number of
   * URIs below. */
  int32_t index = 0;
  for (; index < URI_VCF_VC_PREFIX; index++)
    if (! config.uris[index]) break;

  if (index < URI_VCF_VC_PREFIX)
    return (ui_print_redland_error () == 0);

  config.nodes[NODE_RDF_TYPE]                 = new_node (config.uris[URI_RDF_PREFIX], "type");
  config.nodes[NODE_ORIGIN_CLASS]             = new_node (config.uris[URI_ONTOLOGY_PREFIX], "Origin");
  config.nodes[NODE_VCF_HEADER_GENERIC_CLASS] = new_node (config.uris[URI_ONTOLOGY_PREFIX], "VcfHeaderGenericItem");
  config.nodes[NODE_VCF_HEADER_INFO_CLASS]    = new_node (config.uris[URI_ONTOLOGY_PREFIX], "VcfHeaderInfoItem");
  config.nodes[NODE_VCF_HEADER_FORMAT_CLASS]  = new_node (config.uris[URI_ONTOLOGY_PREFIX], "VcfHeaderFormatItem");
  config.nodes[NODE_VCF_HEADER_FILTER_CLASS]  = new_node (config.uris[URI_ONTOLOGY_PREFIX], "VcfHeaderFilterItem");
  config.nodes[NODE_VCF_HEADER_ALT_CLASS]     = new_node (config.uris[URI_ONTOLOGY_PREFIX], "VcfHeaderAltItem");
  config.nodes[NODE_VCF_HEADER_CONTIG_CLASS]  = new_node (config.uris[URI_ONTOLOGY_PREFIX], "VcfHeaderContigItem");
  config.nodes[NODE_SAMPLE_CLASS]             = new_node (config.uris[URI_ONTOLOGY_PREFIX], "Sample");
  config.nodes[NODE_VARIANT_CLASS]            = new_node (config.uris[URI_ONTOLOGY_PREFIX], "Variant");

  /* NOTE: Keep the number of NODEs defined above in sync with the number of
   * NODEs below. */
  index = 0;
  for (; index < NODE_VARIANT_CLASS; index++)
    if (! config.nodes[index]) break;

  if (index < NODE_VARIANT_CLASS)
    return (ui_print_redland_error () == 0);

  config.types[TYPE_STRING]  = new_uri ("http://www.w3.org/2001/XMLSchema#string");
  config.types[TYPE_INTEGER] = new_uri ("http://www.w3.org/2001/XMLSchema#integer");
  config.types[TYPE_FLOAT]   = new_uri ("http://www.w3.org/2001/XMLSchema#float");
  config.types[TYPE_BOOLEAN] = new_uri ("http://www.w3.org/2001/XMLSchema#boolean");
  if (! (config.types[TYPE_STRING]
         && config.types[TYPE_INTEGER]
         && config.types[TYPE_FLOAT]
         && config.types[TYPE_BOOLEAN]))
    return (ui_print_redland_error () == 0);

  config.rdf_serializer = librdf_new_serializer (config.rdf_world, "ntriples", NULL, NULL);

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
    {
      librdf_free_uri (config.uris[index]);
      config.uris[index] = NULL;
    }

  /* Free the memory of the NODES. */
  for (index = 0; index < NUMBER_OF_NODES; index++)
    {
      librdf_free_node (config.nodes[index]);
      config.nodes[index] = NULL;
    }

  /* Free the memory of the TYPES. */
  for (index = 0; index < NUMBER_OF_TYPES; index++)
    {
      librdf_free_uri (config.types[index]);
      config.types[index] = NULL;
    }

  /* Free the memory of the RDF world. */
  librdf_free_serializer (config.rdf_serializer);
  config.rdf_serializer = NULL;
  librdf_free_storage (config.rdf_storage);
  config.rdf_storage = NULL;
  librdf_free_model (config.rdf_model);
  config.rdf_model = NULL;
  librdf_free_world (config.rdf_world);
  config.rdf_world = NULL;

  /* Free caches. */
  if (config.info_field_indexes != NULL)
    {
      free (config.info_field_indexes);
      config.info_field_indexes = NULL;
    }
}

bool
generate_variant_id (char *variant_id)
{
  int32_t bytes_written;
  bytes_written = snprintf (variant_id, 16, "UV%010u",
                            config.non_unique_variant_counter);

  config.non_unique_variant_counter++;
  return (bytes_written > 0);
}

void refresh_model (void)
{
  runtime_configuration_free ();
  if (! runtime_configuration_redland_init ())
    ui_print_redland_error ();
}
