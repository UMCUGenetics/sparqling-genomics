# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS 	= subdir-objects
SUBDIRS                 = . 

doc_NAME                = sparqling-genomics
doc_PDF                 = $(doc_NAME).pdf
doc_TEX                 = $(doc_NAME).tex
doc_STYLE		= umcu.sty

if HAVE_PDFLATEX
dist_doc_DATA           = $(doc_PDF)

# Run pdflatex twice, so that the table of contents and references are
# properly set up.
$(doc_PDF):
	$(PDFLATEX) $(doc_TEX) $(doc_STYLE)
	$(PDFLATEX) $(doc_TEX) $(doc_STYLE)
endif

EXTRA_DIST              = $(doc_TEX) $(doc_STYLE) 	\
			  introduction.tex		\
			  figures/logo.pdf		\
			  figures/web-connections.png	\
			  figures/web-query.png

docs-clean:
	$(RM) -f $(doc_NAME).aux \
                 $(doc_NAME).log \
                 $(doc_NAME).out \
                 $(doc_NAME).toc

clean-local: docs-clean

# Make sure there is something to run.
if HAVE_PDFLATEX
all: $(doc_PDF)
endif
