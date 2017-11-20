(define-module (www pages welcome)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-welcome))

(define %db-statistics-query
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX : <http://localhost:5000/StructuralVariants/>

SELECT COUNT(DISTINCT ?filename) AS ?numberOfFiles, COUNT(DISTINCT ?variant) AS ?numberOfVariants
{
  ?origin a :Origin .
  ?origin :filename ?filename .
  ?variant a :Variant .
  ?variant :origin ?origin .
}")

(define %db-count-query
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX : <http://localhost:5000/StructuralVariants/>

SELECT ?filename, COUNT(DISTINCT ?variant) AS ?numberOfVariants
{
  ?origin a :Origin .
  ?origin :filename ?filename .
  ?variant a :Variant .
  ?variant :origin ?origin .
}")

(define (database-statistics)
  (catch 'system-error
    (lambda _
      (receive (header port) (sparql-query %db-statistics-query #:type "text/csv")
        (if (= (response-code header) 200)
            (begin
              ;; The first line is the header.
              (read-line port)
              (map string->number (csv-split-line (read-line port) #\,)))
            '(0 0))))
    (lambda (key . args)
      '())))

(define* (process-count-results port #:optional (results '()))
  (let ((line (read-line port)))
    (if (eof-object? line)
        results
        (process-count-results port
                               (cons (string->number
                                      (cadr (csv-split-line line #\,)))
                                     results)))))

(define (variant-count)
  (catch 'system-error
    (lambda _
      (receive (header port) (sparql-query %db-count-query #:type "text/csv")
        (if (= (response-code header) 200)
            (begin
              ;; The first line is the header.
              (read-line port)
              (apply + (process-count-results port)))
            0)))
    (lambda (key . args) 0)))

(variant-count)

(define* (page-welcome request-path #:key (post-data ""))
  (let ((plot-file (string-append %www-static-root
                                  "/images/mutations-per-type.svg"))
        (external (string-append "/static/images/mutations-per-type.svg"))
        (statistics (database-statistics))
        (validation-count (variant-count)))
    (page-root-template "sparqling-svs" request-path
     `((h2 "Overview")
       ,(if (null? statistics)
            `(p "There is a problem connecting to the database.")
            `(p "The database contains " ,(car statistics)
                ,(if (= (car statistics) 1) " file" " files") " and "
                ,validation-count " variants, of which " ,(cadr statistics)
                " are distinct."))))))
