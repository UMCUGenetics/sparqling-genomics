# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS 	= subdir-objects
SUBDIRS                 = . 
AM_CFLAGS 		= $(LIBGCRYPT_CFLAGS) $(htslib_CFLAGS)
bin_PROGRAMS            = vcf2turtle
vcf2turtle_SOURCES      = src/vcf2turtle.c src/RuntimeConfiguration.h   \
                          src/GenomePosition.c src/GenomePosition.h     \
                          src/Variant.c src/Variant.h                   \
                          src/Sample.c src/Sample.h                     \
                          src/Origin.c src/Origin.h                     \
                          src/VcfHeader.c src/VcfHeader.h               \
                          src/helper.c src/helper.h

vcf2turtle_LDFLAGS      = -pthread
vcf2turtle_LDADD        = $(LIBGCRYPT_LIBS) $(htslib_LIBS)
