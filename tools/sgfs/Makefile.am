# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS = subdir-objects
SUBDIRS          = . guile

bin_PROGRAMS     = sgfs

sgfs_CFLAGS      = $(fuse_CFLAGS) $(guile_CFLAGS)
sgfs_LDADD       = $(fuse_LIBS) $(guile_LIBS)
sgfs_SOURCES     = src/sgfs.c
