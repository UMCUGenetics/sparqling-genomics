(define-module (www pages getting-started)
  #:use-module (www pages)
  #:use-module (www config)
  #:export (page-getting-started))

(define (page-getting-started request-path)
  (page-root-template "sparqling-svs" request-path
   `((h2 "Getting started")
     (h3 "Accessing the database")
     (p "You can access the database using the "
        (a (@ (href "/query")) "query interface")
        ", or directly using the "
        (a (@ (href ,(string-append "http://" %sparql-endpoint-host ":"
                                    (number->string %sparql-endpoint-port)
                                    "/sparql")))
           "SPARQL endpoint URL") "."))))
