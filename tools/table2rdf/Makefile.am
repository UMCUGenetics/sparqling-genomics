# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS     = subdir-objects
SUBDIRS              = .
AM_CFLAGS            = -Iinclude $(LIBGCRYPT_CFLAGS) $(raptor2_CFLAGS)

if ENABLE_MTRACE_OPTION
AM_CFLAGS              += -DENABLE_MTRACE
endif

bin_PROGRAMS         = table2rdf
table2rdf_SOURCES    = src/helper.c include/helper.h                          \
                       src/main.c include/runtime_configuration.h             \
                       src/runtime_configuration.c                            \
                       src/ui.c include/ui.h                                  \
                       src/ontology.c include/ontology.h                      \
                       src/table.c include/table.h

table2rdf_LDFLAGS    = # -pthread
table2rdf_LDADD      = $(LIBGCRYPT_LIBS) $(raptor2_LIBS)
