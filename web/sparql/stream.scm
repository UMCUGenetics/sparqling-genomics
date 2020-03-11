;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (sparql stream)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (sparql util)
  #:use-module (srfi srfi-1)

  #:export (csv->scm-stream
            csv->json-stream
            csv->xml-stream
            csv-stream))

;; ----------------------------------------------------------------------------
;; Query result streaming
;; ----------------------------------------------------------------------------
;;
;; This module implements streaming HTTP 1.1 I/O for query results in CSV
;; format to S-expression, JSON or XML.
;;

;; Helper functions
;; ----------------------------------------------------------------------

;; Borrowed from (www db api).
(define-syntax-rule (is-format a b)
  (or (equal? a b)
      (member a b)))

;; Stream functions
;; ----------------------------------------------------------------------

(define* (csv->scm-stream input-port output-port #:optional (header '()))
  "Read the query response from PORT and turn it into an S-expression."

  (format output-port "Content-Type: text/csv\n\n")
  (let [(tokens (csv-read-entry input-port #\,))]
    (if (null? tokens)
        (format output-port "~a" (if (null? header) "()" ")"))
        ;; The first line in the output is the table header.
        (begin
          (if (null? header)
              (begin
                (format output-port "(")
                (set! header tokens))
              (let* [(pairs (zip header tokens))
                     (first (car pairs))]
                (format output-port "((~a . ~a)"
                        (list-ref first 0)
                        (list-ref first 1))
                (for-each (lambda (pair)
                            (format output-port " (~a: ~a)"
                                    (list-ref pair 0)
                                    (list-ref pair 1)))
                          (cdr pairs))
                (format output-port ") ")))
          (csv->scm-stream input-port output-port header)))))

(define* (csv->json-stream input-port output-port #:optional (header '()))
  "Read the query response from PORT and turn it into JSON."
  (let [(tokens (csv-read-entry input-port #\,))]
    (if (null? tokens)
        (format output-port "~a" (if (null? header) "[]" "]"))
        ;; The first line in the output is the table header.
        (begin
          (if (null? header)
              (begin
                (format output-port "[")
                (set! header tokens))
              (let* [(pairs (zip header tokens))
                     (first (car pairs))]
                (format output-port "{ ~s: ~a "
                        (list-ref first 0)
                        (if (string->number (list-ref first 1))
                            (list-ref first 1)
                            (format #f "~s" (list-ref first 1))))

                (for-each (lambda (pair)
                            (format output-port ", ~s: ~a "
                                    (list-ref pair 0)
                                    (if (string->number (list-ref pair 1))
                                        (list-ref pair 1)
                                        (format #f "~s" (list-ref pair 1)))))
                          (cdr pairs))
                (format output-port "}")))
          (csv->json-stream input-port output-port header)))))

(define* (csv->xml-stream input-port output-port #:optional (header '()))
  "Read the query response from PORT and turn it into XML."
  (let [(tokens (csv-read-entry input-port #\,))]
    (if (null? tokens)
        (format output-port "~a" (if (null? header)
                                     "<results></results>"
                                     "</results>"))
        ;; The first line in the output is the table header.
        (begin
          (if (null? header)
              (begin
                (format output-port "<results>")
                (set! header tokens))
              (let* [(pairs (zip header tokens))
                     (first (car pairs))]
                (format output-port "<result><~a>~a</~a>"
                        (list-ref first 0)
                        (list-ref first 1)
                        (list-ref first 0))
                (for-each (lambda (pair)
                            (format output-port "<~a>~a</~a>"
                                    (list-ref pair 0)
                                    (list-ref pair 1)
                                    (list-ref pair 0)))
                          (cdr pairs))
                (format output-port "</result>")))
          (csv->xml-stream input-port output-port header)))))

(define (csv->csv-stream input-port output-port)

  (define (send-header-line line)
    (format output-port "~a\n" line))

  (for-each send-header-line '("Content-Type: text/csv" ""))

  (let* [(buffer-size (expt 2 12))
         (buffer      (make-bytevector buffer-size))
         (eof-yet?    #f)]

    (while (not eof-yet?)
      (let [(nbytes (get-bytevector-some! input-port buffer 0 buffer-size))]
        (if (eof-object? nbytes)
            (set! eof-yet? #t)
            (begin
              (format output-port "~a\r\n" nbytes)
              (put-bytevector output-port buffer 0 nbytes)
              (format output-port "\r\n")))))

    (format output-port "0\r\n\r\n")))

(define (csv-stream input-port output-port fmt)
  "Stream CSV data from INPUT-PORT to OUTPUT-PORT using output format FMT."

  ;; This encoding makes sure that one character is equal to one byte.
  (set-port-encoding! output-port "ISO-8859-1")

  ;; Build the HTTP header.
  (format output-port "~{~a\n~}"
          '("HTTP/1.1 200 OK"
            "Server: SPARQLing-genomics"
            "Connection: close"
            "Transfer-Encoding: chunked"))

  ;; Choose the corresponding stream function.
  (let [(stream-function
         (cond
          [(is-format '(application/json) fmt)         csv->json-stream]
          [(is-format '(application/xml) fmt)          csv->xml-stream]
          [(is-format '(application/s-expression) fmt) csv->scm-stream]
          [(is-format '(text/csv) fmt)                 csv->csv-stream]
          [else #f]))]
    (if stream-function
        (stream-function input-port output-port)
        #f)))
