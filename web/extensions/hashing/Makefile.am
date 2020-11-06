AUTOMAKE_OPTIONS          = subdir-objects

extensiondir = $(EXTDIR)
extension_LTLIBRARIES     = libhashing.la

libhashing_la_CFLAGS   = -Iinclude/ $(guile_CFLAGS) $(gnutls_CFLAGS)
libhashing_la_LIBADD   = $(guile_LIBS) $(gnutls_LIBS)
libhashing_la_SOURCES  = src/hashing.c include/hashing.h
