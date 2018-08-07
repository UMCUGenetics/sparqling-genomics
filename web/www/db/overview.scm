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
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)

  #:export (number-of-samples
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

(define* (number-of-samples #:key (connection #f))
  (catch #t
    (lambda _
      (let* ((query "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sg: <http://rdf.umcutrecht.nl/vcf2rdf/>
PREFIX nsg: <http://rdf.op.umcutrecht.nl/>

SELECT DISTINCT ?sample
WHERE
{
  ?sample rdf:type sg:Sample .
}")
             (results (single-value-query query #:connection connection)))
        (if (list? results)
            (apply + (map length results))
            (length results))))
    (lambda (key . args) 0)))

;; NUMBER-OF-VARIANT-CALLS
;; ----------------------------------------------------------------------------

(define* (number-of-variant-calls #:key (connection #f))
  (catch #t
    (lambda _
      (let* ((query "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX sg: <http://rdf.umcutrecht.nl/vcf2rdf/>
PREFIX nsg: <http://rdf.umcutrecht.nl/vcf2rdf/>

SELECT COUNT(DISTINCT ?variant)
WHERE
{
  ?variant rdf:type sg:VariantCall .
}")
             (results (single-value-query query #:connection connection)))
        (if (list? results)
            (apply + (map string->number (delete #f results)))
            (string->number results))))
    (lambda (key . args) 0)))

