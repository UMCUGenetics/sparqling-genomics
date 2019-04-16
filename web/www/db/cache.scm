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

(define-module (www db cache)
  #:use-module (ice-9 receive)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (srfi srfi-1)
  #:use-module (web response)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www util)

  #:export (cache-clear
            cache-value
            cached-value))

;; CACHED-VALUE
;; ----------------------------------------------------------------------------
;;
;; This function retrieves a value from the cache graph.
;;

(define (cached-value username connection property)
  (let ((query (format #f "PREFIX web: <http://sparqling-genomics/web/>
PREFIX : <http://sparqling-genomics/web/cache/>

SELECT ?value FROM <http://~a/sg-cache>
WHERE {
  web:cache :~a ?value .
}" username property)))
    (catch #t
      (lambda _
        (let ((results (query-results->list
                        (system-sparql-query query)
                        #t)))
          (if (null? results)
              #f
              results)))
      (lambda (key . args)
        #f))))

;; CACHE-VALUE
;; ----------------------------------------------------------------------------
;;
;; This function stores a value into the cache graph.
;;

(define (cache-value username connection property value)
  (if (list? value)
      (map (lambda (item)
             (cache-value username connection property item))
           value)
      (receive (header port)
          (system-sparql-query
           (format #f "PREFIX : <http://sparqling-genomics/web/cache/>
INSERT INTO <http://~a/sg-cache> {
  <http://sparqling-genomics/web/cache> :~a ~s .
}" username property value))
        (= (response-code header) 200))))

;; CACHE-CLEAR
;; ----------------------------------------------------------------------------
;;
;; This function removes all cached items.
;;

(define (cache-clear username)
  (receive (header port)
      (system-sparql-query
       (format #f "DEFINE sql:log-enable 3
CLEAR GRAPH <http://~a/sg-cache>" username))
    (= (response-code header) 200)))
