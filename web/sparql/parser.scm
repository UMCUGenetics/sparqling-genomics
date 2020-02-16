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
  #:use-module (ice-9 match)
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

            query-global-graphs
            set-query-global-graphs!

            query-out-variables
            set-query-out-variables!

            parse-query))

(define-class <query> ()

  (type            #:init-value #nil
                   #:getter query-type
                   #:setter set-query-type!)

  (base            #:init-value #f
                   #:getter query-base
                   #:setter set-query-base!)

  (prefixes        #:init-value '()
                   #:getter query-prefixes
                   #:setter set-query-prefixes!)

  (global-graphs   #:init-value '()
                   #:getter query-global-graphs
                   #:setter set-query-global-graphs!)

  (out-variables   #:init-value '()
                   #:getter query-out-variables
                   #:setter set-query-out-variables!)

  (triple-patterns #:init-value '()
                   #:getter query-triple-patterns
                   #:setter set-query-triple-patterns!))


(define-method (write (query <query>) out)
  (format out "#<<query> ~a, prefixes: ~a>, global graphs: ~a"
          (query-type query)
          (length (query-prefixes query))
          (length (query-global-graphs query))))

(define (parse-query query)
  "Returns an instace of <query>."

  (define (string-is-longer-than str length)
    (catch 'out-of-range
      (lambda _ (if (string-ref str length) #t))
      (lambda (key . args) #f)))

  (define* (string-pred-index pred str #:optional (start 0))
    (if (pred (string-ref str start))
        start
        (string-pred-index pred str (+ start 1))))

  (define* (string-non-pred-index pred str #:optional (start 0))
    (if (not (pred (string-ref str start)))
        start
        (string-non-pred-index pred str (+ start 1))))

  (define* (is-absolute-uri? str)
    (if (and (string? str)
             (string-contains str "://"))
        #t
        #f))

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
              (let [(out-token (string-copy token (+ uri-start 1) uri-end))]
                (if (and (query-base out)
                         (not (is-absolute-uri? out-token)))
                    (string-append (query-base out) out-token)
                    out-token))
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


  (define* (remove-comments text #:optional (position 0)
                                            (modes    '(none))
                                            (output   '()))
    (if (or (not position)
            (not (string-is-longer-than text position)))
        (list->string (reverse output))
        (let [(buffer (string-ref text position))]
          (cond
           [(eq? buffer #\#)
            (if (or (eq? (car modes) 'single-quoted)
                    (eq? (car modes) 'double-quoted))
                (remove-comments text
                                 (+ position 1)
                                 modes
                                 (cons buffer output))
                (remove-comments text
                                 (string-index text #\newline position)
                                 modes
                                 output))]
           [(eq? buffer #\")
            (remove-comments text
                             (+ position 1)
                             (if (eq? (car modes) 'double-quoted)
                                 (cdr modes)
                                 (cons 'double-quoted modes))
                             (cons buffer output))]
           [(eq? buffer #\')
            (remove-comments text
                             (+ position 1)
                             (if (eq? (car modes) 'single-quoted)
                                 (cdr modes)
                                 (cons 'single-quoted modes))
                             (cons buffer output))]
           [else
            (remove-comments text
                             (+ position 1)
                             modes
                             (cons buffer output))]))))

  (define* (read-base out text #:optional (start 0))
    ;; We expect the URI to come after BASE to be an absolute URI because
    ;; this is what the SPARQL spec has to say about it:
    ;; “Base IRIs declared with the BASE keyword must be absolute IRIs.”.
    (let* [(base-start  (string-contains-ci-surrounded-by-whitespace
                         text "base" start))
           (uri-start   (when base-start
                          (string-index text #\< (+ base-start 4))))
           (uri-end     (unless (unspecified? uri-start)
                          (string-index text #\> (+ uri-start 1))))]
      (if (unspecified? uri-end)
          start
          (begin
            (set-query-base! out (string-copy text (+ uri-start 1) uri-end))
            (+ uri-end 1)))))

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
          (let [(uri-token (parse-uri-token out
                             (string-trim-both
                              (string-copy text uri-start (+ uri-end 1)))))]
            (set-query-prefixes! out
              (cons `(;; Cut out the shortcode.
                      ,(string-trim-both
                        (string-copy text (+ prefix-start 7) shortcode-end))
                      .
                      ;; Cut out the URI.
                      ,uri-token)
                    (query-prefixes out)))
            (read-prefixes out text (+ uri-end 1))))))

  (define (read-prologue out query start)
    (let* [(after-base     (read-base out query 0))
           (after-prefixes (read-prefixes out query 0))]
      after-prefixes))

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
           (cars  (map car types))
           (type  (cond
                   [(null? cars)          'UNKNOWN]
                   [(> (length cars) 1)   (apply symbol-append cars)]
                   [else                  (car cars)]))]
      (set-query-type! out type)
      (if (eq? type 'UNKNOWN)
          start
          (assoc-ref types type))))

  (define (cons-token out lst tokens)
    (let [(token     (list->string (reverse lst)))
          (from-test (and (not (null? tokens))
                          (string? (car tokens))
                          (string-ci= (car tokens) "from")))]
      (if (or (string= token "")
              (and from-test
                   (string-ci= token "named")))
          tokens
          (if from-test
              (cons `(,(car tokens) . ,(parse-uri-token out token))
                    (cdr tokens))
              (cons token tokens)))))

  (define* (tokenize-select-query out text #:optional (cursor  0)
                                                      (modes   '(none))
                                                      (current '())
                                                      (tokens  '()))
    (if (or (not cursor)
            (not (string-is-longer-than text cursor)))
        tokens
        (let [(buffer (string-ref text cursor))]
          (cond
           [(eq? buffer #\()
            (tokenize-select-query out text (+ cursor 1)
                                  (cons 'in-function modes)
                                  current
                                  tokens)]
           [(and (eq? buffer #\))
                 (eq? (car modes) 'in-function))
            (tokenize-select-query out text (+ cursor 1)
                                  (cdr modes)
                                  '()
                                  (cons-token out current tokens))]
           [(eq? buffer #\")
            (tokenize-select-query out text
                             (+ cursor 1)
                             (if (eq? (car modes) 'double-quoted)
                                 (cdr modes)
                                 (cons 'double-quoted modes))
                             (cons buffer current)
                             tokens)]
           [(eq? buffer #\')
            (tokenize-select-query out text
                             (+ cursor 1)
                             (if (eq? (car modes) 'single-quoted)
                                 (cdr modes)
                                 (cons 'single-quoted modes))
                             (cons buffer current)
                             tokens)]
           [(and (char-whitespace? buffer)
                 (or (eq? (car modes) 'none)
                     (eq? (car modes) 'in-function)))
            (tokenize-select-query out text (+ cursor 1)
                                  modes
                                  '()
                                  (cons-token out current tokens))]
           [(and (eq? buffer #\{)
                 (eq? (car modes) 'none))
            (cons (list->string (reverse current)) tokens)]
           [else
            (tokenize-select-query out text (+ cursor 1)
                                   modes
                                   (cons buffer current)
                                   tokens)]))))

  (define (read-global-graphs out tokens)
    (for-each (lambda (item)
                (match item
                  (("FROM" . uri)
                   (cond
                    [(is-absolute-uri? uri)
                     (set-query-global-graphs! out
                       (cons uri (query-global-graphs out)))]
                    [else
                     (set-query-global-graphs! out
                       (cons (parse-uri-token out uri)
                             (query-global-graphs out)))]))
                  (else #f)))
              tokens))

  (define (parse-select-query out query cursor)
    (let [(tokens (tokenize-select-query out query cursor))]
      (read-global-graphs out tokens)))

  (let* [(out (make <query>))]
    ;; The following functions write their findings to ‘out’ as side-effects.
    (let* [(q              (remove-comments query))
           (after-prologue (read-prologue out q 0))
           (type-position  (determine-query-type out q after-prologue))]
      (match (query-type out)
        ('ASK           #f)
        ('CLEAR         #f)
        ('CONSTRUCT     #f)
        ('DELETE        #f)
        ('DELETEINSERT  #f)
        ('DESCRIBE      #f)
        ('INSERT        #f)
        ('SELECT        (parse-select-query out query type-position))
        (else           (format #t "Doesn't match anything.~%"))))
    out))


