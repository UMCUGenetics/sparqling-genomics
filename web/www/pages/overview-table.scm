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

(define-module (www pages overview-table)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www db overview)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)

  #:export (page-overview-table))

(define (%query-endpoint username)
  (let ((connections (all-connections username)))
    (if (null? connections)
        #f
        (connection-name (car connections)))))

(define %number-of-samples-query
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sg: <http://sparqling-genomics/>

SELECT (COUNT(DISTINCT ?sample) AS ?samples)
WHERE {
  ?sample rdf:type sg:Sample .
}
")

(define %number-of-variant-calls-query
  "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX vcf2rdf: <http://sparqling-genomics/vcf2rdf/>

SELECT (COUNT(?variant) AS ?variants)
WHERE {
  ?variant rdf:type vcf2rdf:VariantCall .
}")

(define %number-of-copynumber-calls-query
  "PREFIX col: <http://sparqling-genomics/table2rdf/Column/>

SELECT (COUNT(?cnv) AS ?cnvs)
WHERE {
  ?cnv col:copynumber ?o .
}")

(define (make-query-button text query username)
  (if (%query-endpoint username)
      `(form (@ (action "/query") (method "post"))
             (input (@ (type "hidden")
                       (name "endpoint")
                       (value ,(%query-endpoint username))))
             ,(string-append text " ")
             (button (@ (type "submit")
                        (class "small-action-btn question-btn")
                        (name "query")
                        (value ,query))
                     "SHOW ME"))
      text))

(define* (page-overview-table request-path username #:key (post-data ""))
  (let* ((info (par-map (lambda (func) (func username))
                        (list number-of-samples
                              number-of-variant-calls
                              number-of-copynumber-calls))))
    `(table (@ (id "overview-table")
               (class "item-table"))
      (tr (th "Property")
          (th "Value"))
      (tr (td ,(make-query-button
                "Number of samples"
                %number-of-samples-query
                username))
          (td ,(list-ref info 0)))
      (tr (td ,(make-query-button
                "Number of variant calls (may contain duplicates)"
                %number-of-variant-calls-query
                username))
          (td ,(list-ref info 1)))
      (tr (td ,(make-query-button
                "Number of copy number calls (may contain duplicates)"
                %number-of-copynumber-calls-query
                username))
          (td ,(list-ref info 2))))))
