;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2016  Ricardo Wurmus <rekado@elephly.net>
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

(define-module (www util)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (sparql driver)
  #:use-module (srfi srfi-1)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www config)
  #:export (csv-split-line
            file-extension
            predicate-label
            sparql-response->sxml
            string-replace-occurrence
            suffix-iri
            string-is-longer-than
            post-data->alist
            alist-symbol-key<?
            mkdir-p))

(define (string-is-longer-than str length)
  (catch 'out-of-range
    (lambda _ (if (string-ref str length) #t))
    (lambda (key . args) #f)))

(define (file-extension file-name)
  (last (string-split file-name #\.)))

(define (string-replace-occurrence str occurrence alternative)
  "Replaces every OCCURRENCE in STR with ALTERNATIVE."
  (string-map (lambda (x) (if (eq? x occurrence) alternative x)) str))

(define (csv-split-line line delimiter)
  "Splits LINE where DELIMITER is the separator, properly handling quotes."

  (define (iterator line length token-begin position in-quote tokens)
    (cond
     ((= position length)
      (reverse (cons (string-drop line token-begin) tokens)))
     ((eq? (string-ref line position) delimiter)
      (if in-quote
          (iterator line length token-begin (1+ position) in-quote tokens)
          (iterator line length (1+ position) (1+ position) in-quote
                    (cons (substring line token-begin position)
                          tokens))))
     ((eq? (string-ref line position) #\")
      (iterator line length token-begin (1+ position) (not in-quote) tokens))
     (else (iterator line length token-begin (1+ position) in-quote tokens))))

  (iterator line (string-length line) 0 0 #f '()))

(define (suffix-iri input)
  (if input
      (string-trim-both
       (string-drop input
                    (1+ (string-rindex input #\/))) #\")
      "unknown"))

(define (predicate-label pred)
  "Returns the rdf:label of PRED, or PRED if rdf:label is unavailable."
  (catch 'system-error
    (lambda _
      (receive (header port)
          (sparql-query
           (format #f "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?label { <~a> rdf:label ?label } LIMIT 1" (string-trim-both pred #\"))
                        #:type "text/csv")
        (if (= (response-code header) 200)
            (begin
              ;; The first line is the header.
              (read-line port)
              (let ((line (read-line port)))
                (if (or (eof-object? line)
                        (string= line ""))
                    pred
                    (string-trim-both line #\"))))
            pred)))
    (lambda (key . args) pred)))

(define* (sparql-response->sxml port #:optional (body '())
                                                (type #f)
                                                (use-labels? #t))
  "Read the query response from PORT and turn it into a SXML table."

  (define (rdf:type? predicate)
    (string= (string-trim-both predicate #\") 
             "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))

  (let ((line (read-line port)))
    (if (eof-object? line)
        ;; When all data has been processed, we can assemble
        ;; the table by wrapping the HEADER and BODY into a
        ;; table construct.
        (if (null? body)
            (values '() type)
            (values `(table (@ (id "ontology-feature-table"))
                            ,(cons 'thead '((tr (th "Property") (th "Value"))))
                            ,(cons 'tbody (reverse body)))
                    type))
        (let* ((tokens    (csv-split-line line #\,))
               (predicate (list-ref tokens 0))
               (object    (list-ref tokens 1))
               (td-predicate (string-trim-both predicate #\"))
               (td-object-raw (string-trim-both object #\"))
               (td-object (if (and (> (string-length td-object-raw) 6)
                                   (string= "http://" (string-take td-object-raw 7)))
                              `(a (@ (href ,td-object-raw)) ,(suffix-iri td-object-raw))
                              td-object-raw)))
          (if (rdf:type? predicate)
              (sparql-response->sxml port body object)
              (sparql-response->sxml
               port (cons body `((tr (td (a (@ (href ,td-predicate))
                                            ,(if use-labels?
                                                 (predicate-label td-predicate)
                                                 td-predicate)))
                                     (td ,td-object))))
               type))))))

(define (post-data->alist post-data)
  (catch #t
    (lambda _
      (map (lambda (item)
             (let ((pair (string-split item #\=)))
               `(,(string->symbol (uri-decode (car pair))) . ,(uri-decode (cadr pair)))))
           (sort (string-split post-data #\&) string<?)))
    (lambda (key . args) '())))

(define (alist-symbol-key<? a b)
          (string<? (symbol->string (car a))
                    (symbol->string (car b))))

(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))
