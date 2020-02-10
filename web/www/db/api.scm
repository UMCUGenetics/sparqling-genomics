;;; Copyright © 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www db api)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (web response)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (sxml simple)

  #:export (api-serveable-format?
            api-format
            api-request-data->alist
            first-acceptable-format
            alist->sxml))

(define (api-serveable-format? fmt)
  "This function returns #t when FMT can be served, #f otherwise."
  (cond
   [(equal? fmt '(application/json))                       #t]
   [(member '(application/json) fmt)                       #t]
   [(equal? fmt '(application/xml))                        #t]
   [(member '(application/xml) fmt)                        #t]
   [(equal? fmt '(application/s-expression))               #t]
   [(member '(application/s-expression) fmt)               #t]
   [else                                                   #f]))

(define (api-format fmt data)
  (cond
   [(equal? fmt '(application/json))
    (scm->json-string data)]
   [(equal? fmt '(application/xml))
    (call-with-output-string
      (lambda (port) (sxml->xml (alist->sxml data) port)))]
   [(equal? fmt '(application/s-expression))
    (call-with-output-string
      (lambda (port) (write data port)))]
   [else #f]))

(define (first-acceptable-format fmts)
  (if (api-serveable-format? (car fmts))
      (car fmts)
      (first-acceptable-format (cdr fmts))))

(define (api-request-data->alist fmt data)
  "This function parses DATA and returns an ALIST of its contents."
  (cond
   [(or (equal? fmt '(application/json))
        (member 'application/json fmt))
    (let [(json-data (json-string->scm data))]
      ;; This only works for one level of key-value pairs.
      (hash-map->list (lambda (key value)
                        `(,(string->symbol key) . ,value))
                      json-data))]
   [(or (equal? fmt '(application/xml))
        (member 'application/xml fmt))
    ;; It's unclear how this would work without introducing a top-level
    ;; keyword like <parameters>, so that we can write:
    ;; <parameters>
    ;;   <username>...</username>
    ;;   <password>...</password>
    ;; </parameters>
    (map (lambda (item) `(,(car item) . ,(cadr item)))
         (assoc-ref (xml->sxml data) 'parameters))]
   [(or (equal? fmt '(application/s-expression))
        (member 'application/s-expression fmt))
    ;; We want to end up with an S-expression, so we don't need to do
    ;; anything.
    data]
   [(equal? fmt '(application/x-www-form-urlencoded))
    (post-data->alist data)]
   [else #f]))


(define* (alist->sxml input #:optional (inside-list? #f))
  "This function transforms an ALIST or a list of ALISTs into S-expressions
that can be transformed by SXML->XML."
  (cond
   [(and (list? input)
         (list? (car input))
         (not inside-list?))
    `(results
      ,(map (lambda (item)
              (cons 'result
                    (map (lambda (token)
                           (alist->sxml token #t))
                         item)))
            input))]
   [(and (list? input)
         (not inside-list?))
    `(results
      ,(cons 'result
             (map (lambda (item) (alist->sxml item #t)) input)))]
   [else
    (let [(return-match (lambda (a b)
                          (if (string? a)
                              `(,(string->symbol a) ,b)
                              `(,a ,b))))]
      (match input
        ((a b)    (return-match a b))
        ((a . b)  (return-match a b))
        (else     #f)))]))
