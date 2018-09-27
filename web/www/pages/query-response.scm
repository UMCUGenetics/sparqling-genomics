;;; Copyright © 2016, 2017, 2018  Roel Janssen <roel@gnu.org>
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
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (json)
  #:use-module (sxml simple)

  #:export (page-query-response))

(define* (stream-response input-port output-port #:optional (read-header? #t))
  "Read the query response from PORT and turn it into a SXML table."
  (let ((line (read-line input-port)))
    (if (eof-object? line)
        (begin
          (format output-port "</tbody></table>")
          (close-port input-port))
        (let ((tokens (csv-split-line line #\,)))
          ;; The first line in the output is the table header.
          (if read-header?
              (format output-port
                      "<table id=\"query-output\"><thead><tr>~{<th>~a</th>~}</tr></thead><tbody>"
                      (map (lambda (token)
                             (string-trim-both token #\"))
                           tokens))
              (format output-port "<tr>~{<td>~a</td>~}</tr>"
                      (map (lambda (token)
                             (let* ((td-object-raw (string-trim-both token #\"))
                                    (td-object
                                     (if (string-prefix? "http" td-object-raw)
                                         (string-append "<a href=\"" td-object-raw "\">"
                                                        td-object-raw "</a>")
                                         td-object-raw)))
                               td-object))
                           tokens))))))
  (unless (port-closed? input-port)
    (stream-response input-port output-port #f)))

(define* (page-query-response request-path username #:key (post-data ""))

  (define (respond-with-error port)
    (let ((message (get-string-all port)))
      (close-port port)
      `(div (@ (class "query-error"))
            (div (@ (class "title")) "Error")
            (div (@ (class "content"))
                 (pre ,message)))))

  (if (string= post-data "")
      '(p "Please send a POST request with a SPARQL query.")
      (let* ((parsed-data (json-string->scm post-data))
             (connection  (connection-by-name (hash-ref parsed-data "connection") username))
             (backend     (connection-backend connection))
             (query       (hash-ref parsed-data "query"))
             (start-time  (current-time))
             (result
              (catch 'system-error
                (lambda _
                  (receive (header port)
                      (sparql-query query
                                    #:type "text/csv"
                                    #:store-backend backend
                                    #:uri (connection-uri connection)
                                    #:digest-auth
                                    (if (and (connection-username connection)
                                             (connection-password connection))
                                        (string-append
                                         (connection-username connection) ":"
                                         (connection-password connection))
                                        #f))
                    (cond
                     [(= (response-code header) 200)
                      (let* ((end-time (current-time))
                             (time-spent (time-difference end-time start-time))
                             (seconds (+ (time-second time-spent)
                                         (* (time-nanosecond time-spent)
                                            (expt 10 -9) 1.0)))
                             (queries (all-queries username)))
                        (query-add (alist->query
                                    `((endpoint       . ,(connection-name connection))
                                      (content        . ,query)
                                      (execution-time . ,seconds)
                                      (project        . ,(project-name (active-project username))))
                                    queries)
                                   queries
                                   username)
                        (lambda (output-port) (stream-response port output-port)))]
                     [(= (response-code header) 401)
                      (lambda (output-port)
                        (sxml->xml (call-with-input-string "Authentication failed."
                                     respond-with-error) output-port))]
                     [else
                      (lambda (output-port)
                          (sxml->xml (respond-with-error port) output-port))])))
                (lambda (key . args)
                  (if (find (lambda (item)
                              (string= (if (list? item) (car item) item)
                                       "connect"))
                            args)
                      (lambda (output-port)
                        (sxml->xml (call-with-input-string "Failed to connect to the database."
                                     respond-with-error) output-port))
                      (lambda (output-port)
                        (sxml->xml (call-with-input-string
                                         (format #f "An error occurred with details:~%~a~%" args)
                                     respond-with-error) output-port)))))))
        result)))
