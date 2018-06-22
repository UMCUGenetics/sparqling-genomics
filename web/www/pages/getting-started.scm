(define-module (www pages getting-started)
  #:use-module (www pages)
  #:use-module (www config)
  #:export (page-getting-started))

(define (page-getting-started request-path)
  (page-root-template "Getting started" request-path
   `((h2 "Getting started")
     (h3 "Configure a connection")
     (p "Before a query can be performed, a way of connecting to the database"
        " must be configured.  This can be done on the "
        (a (@ (href "/connections")) "connections") " page.")

     (h3 "Querying a database")
     (p "You can query the database using the "
        (a (@ (href "/query")) "query interface") "."))))
