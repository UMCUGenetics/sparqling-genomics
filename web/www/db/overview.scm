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
  #:use-module (www db cache)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (web response)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (logger)

  #:export (number-of-samples
            all-samples
            number-of-variant-calls
            number-of-copynumber-calls))


;; NUMBER-OF-SAMPLES
;; ----------------------------------------------------------------------------

(define* (number-of-samples username #:optional (connection #f))
  (catch #t
    (lambda _
      (let* ((cache-connection (default-connection username))
             (cached (cached-value username cache-connection
                                   "numberofsamples")))
        (cond
         ;; Return the cached value when it exists.
         [cached (car cached)]
         [else
          (let ((result (length (all-samples username connection))))
            (cache-value username cache-connection
                         "numberofsamples" result)
            result)])))
    (lambda (key . args)
      ;; The following cond-construct always returns 0, but
      ;; allows to silence errors that are fully understood.
      (cond
       [(eq? key 'wrong-type-arg) 0]
       [else
        (begin
          (log-error "number-of-samples"
                     "Unknown exception ~a: ~a~%" key args)
          0)]))))

(define* (all-samples username #:optional (connection #f))
  (catch #t
    (lambda _
      (if connection
          (let* ((query "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT DISTINCT ?sample
WHERE { ?sample rdf:type <http://sparqling-genomics/Sample> . }")
                 (results (query-results->list
                           (sparql-query query
                                         #:uri (connection-uri connection)
                                         #:store-backend
                                         (connection-backend connection)
                                         #:digest-auth
                                         (if (and (connection-username connection)
                                                  (connection-password connection))
                                             (string-append
                                              (connection-username connection) ":"
                                              (connection-password connection))
                                             #f))
                           #t)))
            results)
          (let* ((user-connections (all-connections username))
                 (samples (n-par-map (length user-connections)
                                     (lambda (connection)
                                       (all-samples username connection))
                                     user-connections))
                 (simplified-list (stable-sort
                                   (apply append (apply append (delete #f samples)))
                                   string>?)))
            (delete-duplicates-sorted simplified-list string=))))
    (lambda (key . args) 0)))


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

(define* (number-of-variant-calls username #:optional (connection #f))
  (catch #t
    (lambda _
      (let* ((cache-connection (default-connection username))
             (cached (cached-value username cache-connection "variantcalls")))
        (cond
         ;; Return the cached value when it exists.
         [cached (car cached)]
         ;; Return the number of variants for the given connection.
         [connection
          (let* ((query "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sg: <http://sparqling-genomics/vcf2rdf/>

SELECT (COUNT(?variant) AS ?variants) WHERE { ?variant rdf:type sg:VariantCall }")
                 (results (query-results->list
                           (sparql-query query
                                         #:uri (connection-uri connection)
                                         #:store-backend
                                         (connection-backend connection)
                                         #:digest-auth
                                         (if (and (connection-username connection)
                                                  (connection-password connection))
                                             (string-append
                                              (connection-username connection) ":"
                                              (connection-password connection))
                                             #f))
                           #t)))
            (string->number (caar results)))]
         ;; Return the number of variants for all connections.
         [else
          (let* ((user-connections (all-connections username))
                 (variants (delete #f (n-par-map (length user-connections)
                                                 (lambda (connection)
                                                   (number-of-variant-calls
                                                    username connection))
                                                 user-connections)))
                 (result   (apply + variants)))
            (cache-value username cache-connection "variantcalls" result)
            result)])))
    (lambda (key . args) 0)))


;; NUMBER-OF-COPYNUMBER-CALLS
;; ----------------------------------------------------------------------------

(define* (number-of-copynumber-calls username #:optional (connection #f))
  (catch #t
    (lambda _
      (let* ((cache-connection (default-connection username))
             (cached (cached-value username cache-connection "copynumbercalls")))
        (cond
         ;; Return the cached value when it exists.
         [cached (car cached)]
         ;; Return the number of variants for the given connection.
         [connection
          ;; XXX: This query may not be accurate.  It only “works” when only
          ;; table2rdf is used to import copy number variants.
          (let* ((query "PREFIX col: <http://sparqling-genomics/table2rdf/Column/>
SELECT COUNT(?cnv) WHERE { ?cnv col:copynumber ?o }")
                 (results (query-results->list
                           (sparql-query query
                                         #:uri (connection-uri connection)
                                         #:store-backend
                                         (connection-backend connection)
                                         #:digest-auth
                                         (if (and (connection-username connection)
                                                  (connection-password connection))
                                             (string-append
                                              (connection-username connection) ":"
                                              (connection-password connection))
                                             #f))
                           #t)))
            (string->number (caar results)))]
         [else
          (let* ((user-connections (all-connections username))
                 (variants (delete #f (n-par-map (length user-connections)
                                                 (lambda (connection)
                                                   (number-of-copynumber-calls
                                                    username connection))
                                                 user-connections)))
                 (result   (apply + variants)))
            (cache-value username cache-connection "copynumbercalls" result)
            result)])))
    (lambda (key . args) 0)))
