(define-module (www pages welcome)
  #:use-module (www pages)
  #:use-module (www config)
  #:export (page-welcome))

(define* (page-welcome request-path #:key (post-data ""))
  (let ((plot-file (string-append %www-static-root
                                  "/images/mutations-per-type.svg"))
        (external (string-append "/static/images/mutations-per-type.svg")))
    (system* "Rscript"
             (string-append %www-root "/ext/plot_mutations_per_type.R")
             plot-file)
    (page-root-template "sparqling-svs" request-path
     `((h2 "Overview")
       (p "Welcome to " ,%www-name ".  The plots below reflect the current "
          "state of the database.  Therefore, it might've taken a few moments "
          "to load.")
       (h3 "Number of variants per type")
       (p "Each caller specifies which type a variant is.  Please be aware of "
          "the " (a (@ (href "#")) "problem with variant types."))
       (img (@ (src ,external)
               (style "max-height: 250pt; max-width: 400pt")))))))
