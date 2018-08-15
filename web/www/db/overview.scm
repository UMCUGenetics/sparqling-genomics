;;; Copyright © 2018  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (www db overview)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (www db connections)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (web response)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)

  #:export (number-of-samples
            all-samples
            number-of-variant-calls))

;; SINGLE-VALUE-QUERY
;; ----------------------------------------------------------------------------
(define* (single-value-query query #:key (connection #f))
  (if connection
      (let* ((result (catch 'system-error
                       (lambda _
                         (receive (header port)
                             (sparql-query query
                                           #:uri (connection-uri connection)
                                           #:digest-auth
                                           (if (and (connection-username connection)
                                                    (connection-password connection))
                                               (string-append
                                                (connection-username connection) ":"
                                                (connection-password connection))
                                               #f))
                           (if (= (response-code header) 200)
                               (begin
                                 (read-line port) ; Header line)
                                 (read-line port))
                               #f)))
                       (lambda (key . args)
                         #f))))
        result)
      (map (lambda (conn)
             (single-value-query query #:connection conn))
           (all-connections))))


;; NUMBER-OF-SAMPLES
;; ----------------------------------------------------------------------------

(define* (number-of-samples #:optional (connection #f))
  (catch #t
    (lambda _
      (length (all-samples connection)))
    (lambda (key . args)
      (format #t "Thrown exception: ~a: ~a~%" key args)
      0)))

(define* (all-samples #:optional (connection #f))
  (if connection
      (let* ((query "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sg: <http://rdf.umcutrecht.nl/>

SELECT DISTINCT ?sample
WHERE
{
  ?sample rdf:type sg:Sample .
}")
             (results (query-results->list
                       (sparql-query query
                                     #:uri (connection-uri connection)
                                     #:digest-auth
                                     (if (and (connection-username connection)
                                              (connection-password connection))
                                         (string-append
                                          (connection-username connection) ":"
                                          (connection-password connection))
                                         #f))
                       #t)))
        results)
      (delete-duplicates
       (apply append
              (apply append
                     (delete #f
                             (par-map all-samples (all-connections))))))))


;; Wrapper around ‘merge’ that accepts multiple sorted input lists.
;; ----------------------------------------------------------------------------

(define* (merge-multiple less? input #:optional (result '()))
  (if (null? input)
      result
      (merge-multiple less? (cdr input) (merge (car input) result less?))))

;; Function to delete duplicates assuming the list is already sorted.
;; ----------------------------------------------------------------------------

(define* (delete-duplicates-sorted input same? #:optional (result '()))
  (if (null? input)
      (reverse result)
      (let ((item (car input)))
        (if (or (null? result)
                (not (same? (car result) item)))
            (delete-duplicates-sorted (cdr input) same? (cons item result))
            (delete-duplicates-sorted (cdr input) same? result)))))

;; NUMBER-OF-VARIANT-CALLS
;; ----------------------------------------------------------------------------

(define* (number-of-variant-calls #:optional (connection #f))
  (catch #t
    (lambda _
      (if connection
          (let* ((query "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sg: <http://rdf.umcutrecht.nl/vcf2rdf/>

SELECT COUNT(?variant) WHERE { ?variant rdf:type sg:VariantCall }")
                 (results (query-results->list
                           (sparql-query query
                                         #:uri (connection-uri connection)
                                         #:digest-auth
                                         (if (and (connection-username connection)
                                                  (connection-password connection))
                                             (string-append
                                              (connection-username connection) ":"
                                              (connection-password connection))
                                             #f))
                           #t)))
            (string->number (caar results)))
          (let* ((variants (par-map number-of-variant-calls (all-connections))))
            (apply + variants))))
    (lambda (key . args)
      (format #t "Thrown exception: ~a: ~a~%" key args)
      0)))

(define* (number-of-variant-calls-deduplicated #:optional (connection #f))
  (format #t "Pre-executed time:   ~a~%"
          (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
  (catch #t
    (lambda _
      (if connection
          (let* ((query "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sg: <http://rdf.umcutrecht.nl/vcf2rdf/>

SELECT STRAFTER(STR(?variant), 'vcf2rdf/') WHERE { ?variant rdf:type sg:VariantCall }
ORDER BY DESC(?variant)")
                 (results (query-results->list
                           (sparql-query query
                                         #:uri (connection-uri connection)
                                         #:digest-auth
                                         (if (and (connection-username connection)
                                                  (connection-password connection))
                                             (string-append
                                              (connection-username connection) ":"
                                              (connection-password connection))
                                             #f))
                           #t)))
            (map car (delete #f results)))
          ;; To get grand total numbers we need to query each “connection”,
          ;; and remove duplicated entries.  We assume it's faster to let
          ;; the RDF store(s) sort and deduplicate its own results.
          (let* ((variants  (begin
                              (format #t "Executing queries:     ~a~%"
                                      (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
                              (par-map number-of-variant-calls (all-connections))))
                 (merged    (begin
                              (format #t "Merging results:       ~a~%"
                                      (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
                              (merge-multiple string>? variants)))
                 (dedupped  (begin
                              (format #t "Deduplicating results: ~a~%"
                                      (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
                              (delete-duplicates-sorted merged string=))))
            (format #t "Post-executed time: ~a~%"
                    (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
            (length dedupped))))
    (lambda (key . args)
      (format #t "Thrown exception: ~a: ~a~%" key args)
      0)))
