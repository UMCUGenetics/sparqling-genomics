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
  #:use-module (www html)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (sparql driver)
  #:use-module (sparql util)
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
  #:use-module ((fibers) #:prefix fibers:)

  #:export (page-query-response))

(define* (stream-response input-port output-port
                          #:optional (read-header? #t)
                          (number-of-rows 0))
  "Read the query response from PORT and turn it into a SXML table."
  (let [(tokens (csv-read-entry input-port #\,))]
    (if (null? tokens)
        (format output-port "</tbody></table>")
        ;; The first line in the output is the table header.
        (begin
          (if read-header?
              (format output-port
                      "<table id=\"query-output\"><thead><tr>~{<th>~a</th>~}</tr></thead><tbody>"
                      tokens)
              (format output-port "<tr>~{<td>~a</td>~}</tr>"
                      (map (lambda (token)
                             (if (string-prefix? "http" token)
                                 (string-append "<a href=\"" token "\">" token "</a>")
                                 (string-append "<pre>" (sxml->html-string token) "</pre>")))
                           tokens)))
          (unless (>= number-of-rows 5000)
            ;; A call to "sleep" will create a "yield point" for Fibers (the
            ;; threading library).
            (fibers:sleep 0)
            (stream-response input-port output-port #f (+ number-of-rows 1)))))))

(define* (page-query-response request-path username #:key (post-data ""))

  (define (respond-with-error port)
    (let ((message (get-string-all port)))
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
              (catch #t
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
                                            (expt 10 -9) 1.0))))
                        (query-add query
                                   (connection-name connection)
                                   username
                                   seconds
                                   (active-project-for-user username))
                        (lambda (output-port) (stream-response port output-port)))]
                     [(= (response-code header) 401)
                      (lambda (output-port)
                        (sxml->xml (call-with-input-string "Authentication failed."
                                     respond-with-error) output-port))]
                     [else
                      (lambda (output-port)
                        (sxml->xml (respond-with-error port) output-port))])))
                (lambda (key . args)
                  (cond
                   [(eq? key 'system-error)
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
                                       respond-with-error) output-port)))]
                   [else
                    (format #t "Thrown unhandled exception in ~a: ~a: ~a~%"
                            "page-query-response" key args)])))))
        result)))
