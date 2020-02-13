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

(define-module (sparql parser)
  #:use-module (oop goops)
  #:export (<query>

            query-type
            set-query-type!

            query-base
            set-query-base!

            query-prefixes
            set-query-prefixes!

            query-quads
            set-query-quads!

            query-triplets
            set-query-triplets!

            parse-query))

(define-class <query> ()

  (type     #:init-value #nil
            #:getter query-type
            #:setter set-query-type!)

  (base     #:init-value #nil
            #:getter query-base
            #:setter set-query-base!)

  (prefixes #:init-value '()
            #:getter query-prefixes
            #:setter set-query-prefixes!)

  (quads    #:init-value '()
            #:getter query-quads
            #:setter set-query-quads!)

  (triplets #:init-value '()
            #:getter query-triplets
            #:setter set-query-triplets!))


(define-method (write (query <query>) out)
  (format out "#<<query> ~a, prefixes: ~a>"
          (query-type query)
          (length (query-prefixes query))))

(define (parse-query query)
  "Returns an instace of <query>."

  (define (string-contains-ci-surrounded-by-whitespace s1 s2 start)
    (let [(pstart (string-contains-ci s1 s2 start))]
      (cond
       [(not pstart)                            #f]
       [(= pstart 0)                            pstart]
       [(and (> pstart 0)

             (char-whitespace?
              (string-ref s1 (- pstart 1)))

             (catch 'out-of-range
               (lambda _
                 (char-whitespace?
                  (string-ref s1
                    (+ pstart
                       (string-length s2)))))
               (lambda _ #t)))                   pstart]
       [else                                     #f])))

  (define (parse-uri-token out token)
    (if (eq? (string-ref token 0) #\<)

        ;; Deal with absolute URIs.
        ;; --------------------------------------------------------------------
        (let* [(uri-start 0)
               (uri-end   (string-index token #\> (+ uri-start 1)))]
          (if uri-end
              (string-copy token (+ uri-start 1) uri-end)
              #f))

        ;; Deal with shorthand URIs.
        ;; --------------------------------------------------------------------
        (let [(shortcode-end (string-index token #\:))]
          (cond
           [(not shortcode-end)
            #f]
           [(> shortcode-end 0)
            (let* [(sym      (string-trim (string-copy token 0 shortcode-end)))
                   (sym-len  (string-length sym))
                   (prefixes (query-prefixes out))
                   (prefix   (assoc-ref prefixes sym))]
              (cond
               [(= sym-len 0)
                (parse-uri-token out (string-trim token))]
               [prefix
                (string-append prefix (substring token (+ shortcode-end 1)))]
               [else
                #f]))]
           [else
            (let* [(prefixes     (query-prefixes out))
                   (empty-prefix (assoc-ref prefixes ""))]
              (if empty-prefix
                  (string-append empty-prefix (substring token 1))
                  #f))]))))

  (define* (string-pred-index pred str #:optional (start 0))
    (if (pred (string-ref str start))
        start
        (string-pred-index pred str (+ start 1))))

  (define* (string-non-pred-index pred str #:optional (start 0))
    (if (not (pred (string-ref str start)))
        start
        (string-non-pred-index pred str (+ start 1))))

  (define (read-prefixes out text start)
    (let* [(prefix-start  (string-contains-ci-surrounded-by-whitespace
                           text "prefix" start))
           (shortcode-end (when prefix-start
                            (string-index text #\: (+ prefix-start 7))))
           (uri-start     (unless (unspecified? shortcode-end)
                            (let [(uri-open (string-non-pred-index
                                             char-whitespace?
                                             text (+ shortcode-end 1)))]
                              (if uri-open
                                  uri-open
                                  (string-non-pred-index
                                   char-whitespace?
                                   text (+ shortcode-end 1))))))
           (uri-end       (unless (unspecified? uri-start)
                            (if (eq? (string-ref text uri-start) #\<)
                                (string-index text #\> (+ uri-start 1))
                                (string-pred-index char-whitespace?
                                                   text (+ uri-start 1)))))]
      (if (unspecified? uri-end)
          start
          (begin
            (set-query-prefixes! out
              (cons `(;; Cut out the shortcode.
                      ,(string-trim-both
                        (string-copy text (+ prefix-start 7) shortcode-end))
                      .
                      ;; Cut out the URI.
                      ,(parse-uri-token out
                         (string-trim-both
                          (string-copy text uri-start (+ uri-end 1)))))
                    (query-prefixes out)))
            (read-prefixes out text (+ uri-end 1))))))

  (define (determine-query-type out text start)
    (let* [(types  (filter
                    (lambda (item) (cdr item))
                    `((ASK       . ,(string-contains-ci-surrounded-by-whitespace
                                     text "ask" start))
                      (CLEAR     . ,(string-contains-ci-surrounded-by-whitespace
                                     text "clear graph" start))
                      (CONSTRUCT . ,(string-contains-ci-surrounded-by-whitespace
                                     text "construct" start))
                      (DELETE    . ,(string-contains-ci-surrounded-by-whitespace
                                     text "delete" start))
                      (DESCRIBE  . ,(string-contains-ci-surrounded-by-whitespace
                                     text "describe" start))
                      (INSERT    . ,(string-contains-ci-surrounded-by-whitespace
                                     text "insert" start))
                      (SELECT    . ,(string-contains-ci-surrounded-by-whitespace
                                     text "select" start)))))
           (filtered (map car types))
           (type   (if (> (length filtered) 1)
                       (apply symbol-append filtered)
                       (car filtered)))]
      (if (null? types)
          (begin
            (set-query-type! out 'UNKNOWN)
            start)
          (begin
            (set-query-type! out type)
            (assoc-ref types type)))))


  (let* [(out (make <query>))]
    ;; The following functions write their findings to ‘out’ as side-effects.
    (let ((after-prefixes-position (read-prefixes out query 0)))
      (determine-query-type out query after-prefixes-position))
    out))


