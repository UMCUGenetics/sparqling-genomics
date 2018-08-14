;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
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

(define-module (www pages query-response)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (sxml simple)

  #:export (page-query-response))

(define* (response->sxml port
                         #:optional (read-header? #t)
                         (header '())
                         (body '()))
  "Read the query response from PORT and turn it into a SXML table."
  (let ((line (read-line port)))
    (if (eof-object? line)
        ;; When all data has been processed, we can assemble
        ;; the table by wrapping the HEADER and BODY into a
        ;; table construct.
        `(table (@ (id "query-output"))
                ,(cons 'thead header)
                ,(cons 'tbody (reverse body)))
        (let ((tokens (csv-split-line line #\,)))
          ;; The first line in the output is the table header.
          (if read-header?
              (response->sxml port #f
                (append header
                        `((tr ,(map (lambda (token)
                                     `(th ,(string-trim-both token #\")))
                                   tokens))))
                body)
              (response->sxml port #f header
                (cons body
                 `((tr ,(map
                         (lambda (token)
                           (let* ((td-object-raw (string-trim-both token #\"))
                                  (td-object
                                   (if (string-prefix? "http" td-object-raw)
                                       `(a (@ (href ,td-object-raw)) ,td-object-raw)
                                       td-object-raw)))
                             `(td ,td-object)))
                         tokens))))))))))

(define* (page-query-response request-path #:key (post-data ""))

  (define (respond-with-error port)
    `(div (@ (class "query-error"))
          (div (@ (class "title")) "Error")
          (div (@ (class "content"))
               ,(read-line port))))

  (if (string= post-data "")
      '(p "Please send a POST request with a SPARQL query.")
      (let* ((parsed-data (json-string->scm post-data))
             (connection  (connection-by-name (hash-ref parsed-data "connection")))
             (query       (hash-ref parsed-data "query"))
             (result
             (catch 'system-error
               (lambda _
                 (receive (header port)
                     (sparql-query query
                                   #:type "text/csv"
                                   #:uri (connection-uri connection)
                                   #:digest-auth
                                   (if (and (connection-username connection)
                                            (connection-password connection))
                                       (string-append
                                        (connection-username connection) ":"
                                        (connection-password connection))
                                       #f))
                   (if (= (response-code header) 200)
                       (response->sxml port)
                       (respond-with-error port))))
               (lambda (key . args)
                 (if (find (lambda (item)
                             (string= (if (list? item) (car item) item)
                                      "connect"))
                           args)
                     (call-with-input-string "Failed to connect to the database."
                       respond-with-error)
                     (call-with-input-string
                         (format #f "An error occurred with details:~%~a~%" args)
                       respond-with-error))))))
        result)))
