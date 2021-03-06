# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS 	= subdir-objects
SUBDIRS                 = . 

docdir                  = ${pkgdatadir}
doc_NAME                = sparqling-genomics
doc_PDF                 = $(doc_NAME).pdf
doc_HTML                = $(doc_NAME).html
doc_TEX                 = $(doc_NAME).tex
doc_STYLE	        = $(doc_NAME).sty
dist_doc_DATA =

if HAVE_PDFLATEX
dist_doc_DATA           += $(doc_PDF)

# Run pdflatex twice, so that the table of contents and references are
# properly set up.
$(doc_PDF):
	@$(PDFLATEX) $(doc_TEX) $(doc_STYLE) > /dev/null
	@$(BIBTEX) $(doc_NAME) > /dev/null
	@$(PDFLATEX) $(doc_TEX) $(doc_STYLE) > /dev/null
	@$(PDFLATEX) $(doc_TEX) $(doc_STYLE) > /dev/null
	@printf "Generated $(doc_PDF).\n"
endif

EXTRA_DIST              = $(doc_TEX) $(doc_STYLE)                  \
                          command-line-tools.tex                   \
                          figures/layered-knowledge-.png           \
                          figures/layered-knowledge.pdf            \
                          figures/layered-knowledge.xbb            \
                          figures/logo-.png                        \
                          figures/logo.pdf                         \
                          figures/logo.xbb                         \
                          figures/sg-auth-manager-scaleout-.png    \
                          figures/sg-auth-manager-scaleout.pdf     \
                          figures/sg-auth-manager-scaleout.xbb     \
                          figures/sg-web-dashboard-.png            \
                          figures/sg-web-dashboard.pdf             \
                          figures/sg-web-dashboard.xbb             \
                          figures/sg-web-exploratory-.png          \
                          figures/sg-web-exploratory.pdf           \
                          figures/sg-web-exploratory.xbb           \
                          figures/sg-web-form-example-.png         \
                          figures/sg-web-form-example-submit-.png  \
                          figures/sg-web-form-example-submit.pdf   \
                          figures/sg-web-form-example-submit.xbb   \
                          figures/sg-web-form-example.pdf          \
                          figures/sg-web-form-example.xbb          \
                          figures/sg-web-import-rdf-.png           \
                          figures/sg-web-import-rdf.pdf            \
                          figures/sg-web-import-rdf.xbb            \
                          figures/sg-web-project-details-.png      \
                          figures/sg-web-project-details.pdf       \
                          figures/sg-web-project-details.xbb       \
                          figures/sg-web-query-.png                \
                          figures/sg-web-query.pdf                 \
                          figures/sg-web-query.xbb                 \
                          implemented-ontologies.tex               \
                          information-management.tex               \
                          information-retrieval.tex                \
                          introduction.tex                         \
                          knowledge-graph.tex                      \
                          programming-with-sparql.tex              \
                          web-extend.tex                           \
                          web-interface.tex                        \
                          web-programming-interface.tex

if HAVE_HTLATEX
$(doc_HTML): $(doc_PDF)
	@$(HTLATEX) $(doc_TEX) > /dev/null
	@printf "Generated $(doc_HTML).\n"

.PHONY: $(doc_NAME).css $(doc_NAME)10.html $(doc_NAME)2.html
.PHONY: $(doc_NAME)3.html $(doc_NAME)4.html $(doc_NAME)5.html
.PHONY: $(doc_NAME)6.html $(doc_NAME)7.html $(doc_NAME)8.html
.PHONY: $(doc_NAME)9.html


nobase_dist_doc_DATA     = $(doc_HTML)                              \
                           figures/layered-knowledge-.png           \
                           figures/logo-.png                        \
                           figures/sg-auth-manager-scaleout-.png    \
                           figures/sg-web-dashboard-.png            \
                           figures/sg-web-exploratory-.png          \
                           figures/sg-web-form-example-.png         \
                           figures/sg-web-form-example-submit-.png  \
                           figures/sg-web-import-rdf-.png           \
                           figures/sg-web-project-details-.png      \
                           figures/sg-web-query-.png                \
                           $(doc_NAME).css                          \
                           $(doc_NAME)10.html                       \
                           $(doc_NAME)2.html                        \
                           $(doc_NAME)3.html                        \
                           $(doc_NAME)4.html                        \
                           $(doc_NAME)5.html                        \
                           $(doc_NAME)6.html                        \
                           $(doc_NAME)7.html                        \
                           $(doc_NAME)8.html                        \
                           $(doc_NAME)9.html
endif

docs-clean:
	$(RM) -rf $(doc_NAME).aux			   \
		  $(doc_NAME).bbl			   \
		  $(doc_NAME).blg			   \
		  $(doc_NAME).log			   \
		  $(doc_NAME).out			   \
		  $(doc_NAME).toc			   \
		  $(doc_NAME).xref			   \
		  $(doc_NAME).4ct			   \
		  $(doc_NAME).4tc			   \
		  $(doc_NAME).dvi			   \
		  $(doc_NAME).idv			   \
		  $(doc_NAME).tmp			   \
		  $(doc_NAME).lg			   \
		  $(doc_NAME).lof			   \
		  $(doc_NAME).lot			   \
		  $(doc_NAME).pdf                          \
                  $(doc_NAME).css                          \
		  $(doc_NAME).html                         \
                  $(doc_NAME)10.html                       \
                  $(doc_NAME)2.html                        \
                  $(doc_NAME)3.html                        \
                  $(doc_NAME)4.html                        \
                  $(doc_NAME)5.html                        \
                  $(doc_NAME)6.html                        \
                  $(doc_NAME)7.html                        \
                  $(doc_NAME)8.html                        \
                  $(doc_NAME)9.html                        \
                  texput.log

clean-local: docs-clean
