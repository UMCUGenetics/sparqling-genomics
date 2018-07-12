# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS     = subdir-objects
SUBDIRS              = .
AM_CFLAGS            = -Iinclude $(LIBGCRYPT_CFLAGS) $(htslib_CFLAGS)         \
                       $(raptor2_CFLAGS)

if ENABLE_MTRACE_OPTION
AM_CFLAGS              += -DENABLE_MTRACE
endif

bin_PROGRAMS         = vcf2rdf
vcf2rdf_SOURCES      = src/helper.c include/helper.h                          \
                       src/main.c include/runtime_configuration.h             \
                       src/runtime_configuration.c                            \
                       src/ui.c include/ui.h                                  \
                       src/ontology.c include/ontology.h                      \
                       src/vcf_header.c include/vcf_header.h                  \
                       src/vcf_variants.c include/vcf_variants.h

vcf2rdf_LDFLAGS      = # -pthread
vcf2rdf_LDADD        = $(LIBGCRYPT_LIBS) $(htslib_LIBS) $(raptor2_LIBS)
