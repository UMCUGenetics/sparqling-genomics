# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS 	= subdir-objects
SUBDIRS                 = . 

doc_NAME                = sparqling-genomics
doc_PDF                 = $(doc_NAME).pdf
doc_HTML                = $(doc_NAME).html
doc_TEX                 = $(doc_NAME).tex
doc_STYLE		= umcu.sty
dist_doc_DATA =

if HAVE_PDFLATEX
dist_doc_DATA           += $(doc_PDF)

# Run pdflatex twice, so that the table of contents and references are
# properly set up.
$(doc_PDF):
	$(PDFLATEX) $(doc_TEX) $(doc_STYLE)
	$(PDFLATEX) $(doc_TEX) $(doc_STYLE)
endif

if HAVE_HTLATEX
dist_doc_DATA		+= $(doc_NAME).html		\
			   $(doc_NAME)2.html		\
			   $(doc_NAME)3.html		\
			   $(doc_NAME)4.html		\
			   $(doc_NAME).css		\
			   figures/logo.png

$(doc_HTML):
	$(HTLATEX) $(doc_TEX)
endif

EXTRA_DIST              = $(doc_TEX) $(doc_STYLE) 	\
			  version.tex			\
			  introduction.tex		\
			  command-line-tools.tex	\
			  web-interface.tex		\
			  information-retrieval.tex	\
			  programming-with-sparql.tex	\
			  figures/logo.pdf		\
			  figures/logo.png		\
			  figures/web-connections.png	\
			  figures/web-query.png

docs-clean:
	$(RM) -rf $(doc_NAME).aux			\
		  $(doc_NAME).log			\
		  $(doc_NAME).out			\
		  $(doc_NAME).toc			\
		  $(doc_NAME).xref			\
		  $(doc_NAME).4ct			\
		  $(doc_NAME).4tc			\
		  $(doc_NAME).dvi			\
		  $(doc_NAME).idv			\
		  $(doc_NAME).tmp			\
		  $(doc_NAME).lg			\
		  texput.log

clean-local: docs-clean

# Make sure there is something to run.
all: $(doc_PDF) $(doc_HTML)

.PHONY: $(doc_PDF) $(doc_HTML)
