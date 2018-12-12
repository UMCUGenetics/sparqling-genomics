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

  #:export (cache-value
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
          (if (null? results)
              #f
              (begin
                (format #t "[INFO]: Retrieved cached value for '~a'~%" property)
                results))))
      (lambda (key . args)
        (format #t "[INFO]: Couldn't retrieve cached value for '~a'~%" property)
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
          (sparql-query
           (format #f "PREFIX : <http://sparqling-genomics/web/cache/>
INSERT INTO <http://~a/sg-cache> {
  <http://sparqling-genomics/web/cache> :~a ~s .
}" username property value)
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
        (cond
         [(= (response-code header) 200)
          (format #t "[INFO]: Added cache value for '~a'~%" property)
          #t]
         [else
          (format #t "[INFO]: Could not add cache value for '~a'~%" property)
          #f]))))
