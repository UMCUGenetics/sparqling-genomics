AUTOMAKE_OPTIONS          = subdir-objects

extensiondir = $(EXTDIR)
extension_LTLIBRARIES     = libr_report.la

libr_report_la_CFLAGS   = -Iinclude/ $(guile_CFLAGS) $(libr_CFLAGS)
libr_report_la_LIBADD   = $(guile_LIBS) $(libr_LIBS)
libr_report_la_SOURCES  = src/r_report.c include/r_report.h
