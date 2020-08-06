;;; Copyright © 2019, 2020  Roel Janssen <roel@gnu.org>
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

(define-module (test sparql-parser)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs io ports)
  #:use-module (sparql parser)
  #:use-module (srfi srfi-1)

  #:export (run-sparql-parser-test))

;; Some convenience
;; ----------------------------------------------------------------------------
;; Some convenience
;; ----------------------------------------------------------------------------
(define (error . args)
  (let ((port (current-error-port)))
    (apply format (cons port args))
    (newline port)))

(define (success . args)
  (let ((port (current-output-port)))
    (apply format (cons port args))
    (newline port)))

;; The singular query test.
;; ----------------------------------------------------------------------------
(define (test-query filename)
  (catch #t
    (lambda _
      (call-with-input-file filename
        (lambda (port)
          (let* ((query  (get-string-all port))
                 (parsed (parse-query query)))
            (success "Parsed ~s as ~a query."
                     filename (query-type parsed))
            #t))))
    (lambda (key . args)
      (error "Failed to parse ~s with error:~%Thrown: ~a: ~s"
             filename key args)
      #f)))

;; The main entry point for this module.
;; ----------------------------------------------------------------------------
(define (run-sparql-parser-test directory)
  (let* ((files   (scandir directory
                           (lambda (filename)
                             (string-suffix? ".sparql" filename))))
         (results (map test-query (map (lambda (file)
                                         (string-append directory "/" file))
                                       files))))
    (if (any not results)
        (error "~a queries were not parseable."
               (length (delete #t results)))
        (success "All queries could be parsed."))))
