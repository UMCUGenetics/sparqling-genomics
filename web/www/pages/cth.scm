(define-module (www pages cth)
  #:use-module (www pages)
  #:use-module (www pages error)
  #:use-module (www config)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)

  #:export (page-cth))

(define prettify-table
  '(("/cth/type"                      . "Type")
    ("/cth/filter"                    . "Filter")
    ("/cth/quality"                   . "Quality")
    ("/cth/genome_position"           . "Start position")
    ("/cth/genome_position2"          . "End position")
    ("/cth/position"                  . "Position")
    ("/cth/chromosome"                . "Chromosome")
    ("/cth/confidence_interval_start" . "Confidence interval start")
    ("/cth/confidence_interval_end"   . "Confidence interval end")))

(define (without-hostname input)
  (string-drop input (string-length %www-sparql-hostname)))

(define (without-prefix-iri input)
  (string-drop input (+ (string-length %www-sparql-hostname) 5)))

(define (suffix-iri input)
  (if input
      (string-trim-both
       (string-drop input
                    (1+ (string-rindex input #\/))) #\")
      "unknown"))

(define (build-query path)
  (format #f
   "PREFIX : <~a~a/>

SELECT ?predicate ?object
WHERE {
  :~a ?predicate ?object .
}"
   %www-sparql-hostname
   (string-take path (string-rindex path #\/))
   (string-drop path (1+ (string-rindex path #\/)))))

(define* (response->sxml port #:optional (body '()) (type #f))
  "Read the query response from PORT and turn it into a SXML table."

  (define (rdf:type? predicate)
    (string= (string-trim-both predicate #\") 
             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
    
  (define (prettified predicate)
    (let* ((hostname-length (string-length %www-sparql-hostname))
           (pretty (assoc-ref prettify-table
                              (without-hostname (string-trim-both predicate #\")))))
      (if pretty pretty predicate)))

  (let ((line (read-line port)))
    (if (eof-object? line)
        ;; When all data has been processed, we can assemble
        ;; the table by wrapping the HEADER and BODY into a
        ;; table construct.
        (values `(table (@ (id "ontology-feature-table"))
                        ,(cons 'thead '((tr (th "Property") (th "Value"))))
                        ,(cons 'tbody (reverse body)))
                type)
        (let* ((tokens    (string-split line #\,))
               (predicate (list-ref tokens 0))
               (object    (list-ref tokens 1))
               (td-predicate (prettified predicate))
               (td-object-raw (string-trim-both object #\"))
               (td-object (if (and (> (string-length td-object-raw) 6)
                                   (string= "http://" (string-take td-object-raw 7)))
                              `(a (@ (href ,td-object-raw)) ,td-object-raw)
                              td-object-raw)))
          (if (rdf:type? predicate)
              (response->sxml port body object)
              (response->sxml port (cons body
                                    `((tr (td ,td-predicate)
                                          (td ,td-object))))
                              type))))))

(define* (page-cth request-path #:key (post-data ""))
  (let ((result (receive (header port)
                    (sparql-query (build-query request-path) #:type "text/csv")
                  (if (= (response-code header) 200)
                      port
                      #f))))
    (if result
        (begin
          ;; The first line in the output is the table header. Skip it.
          (read-line result)
          (receive (response type)
              (response->sxml result)
            (page-root-template "sparqling-svs" request-path
             `((h2 "Properties of " ,(suffix-iri type))
               ,response))))
        (page-error-404 request-path))))

;;; This query would unfold the genome position data for a given record.
;;; ---------------------------------------------------------------------------
;; (define (build-query prefix identifier
;;   (format #f "PREFIX : <~a>
;; PREFIX sv: <~a/cth/StructuralVariant>

;; SELECT ?type ?filter ?quality ?chromosome1 ?position1 ?cistart1 ?ciend1
;;        ?cistart2 ?ciend2 ?chromosome2 ?position2
;; WHERE {
;;   sv:~a :filter ?filter .
;;   sv:~a :type ?type .
;;   sv:~a :quality ?quality .

;;   sv:~a :genome_position ?p1 .
;;   ?p1 :chromosome ?chromosome1 .
;;   ?p1 :position ?position1 .
;;   ?p1 :confidence_interval_start ?cistart1 .
;;   ?p1 :confidence_interval_end ?ciend1 .

;;   sv:~a :genome_position ?p2 .
;;   ?p2 :chromosome ?chromosome1 .
;;   ?p2 :position ?position1 .
;;   ?p2 :confidence_interval_start ?cistart2 .
;;   ?p2 :confidence_interval_end ?ciend2 .
;; }")))
