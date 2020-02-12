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
;; This module implements streaming I/O for query results in CSV format to
;; S-expression, JSON or XML.
;;

(define* (csv->scm-stream input-port output-port #:optional (header '()))
  "Read the query response from PORT and turn it into an S-expression."
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

(define (csv-stream input-port output-port fmt)
  "Stream CSV data from INPUT-PORT to OUTPUT-PORT using output format FMT."
  (cond
   [(or (equal? '(application/json) fmt)
        (member '(application/json) fmt))
    (csv->json-stream input-port output-port)]
   [(or (equal? '(application/xml) fmt)
        (member '(application/xml) fmt))
    (csv->xml-stream input-port output-port)]
   [(or (equal? '(application/s-expression) fmt)
        (member '(application/s-expression) fmt))
    (csv->scm-stream input-port output-port)]
   [else #f]))
