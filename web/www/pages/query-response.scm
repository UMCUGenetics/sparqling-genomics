(define-module (www pages query-response)
  #:use-module (www pages)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)

  #:export (page-query-response))

(define (suffix-iri input)
  (if input
      (string-trim-both
       (string-drop input
                    (1+ (string-rindex input #\/))) #\")
      "unknown"))

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
        (let ((tokens (string-split line #\,)))
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
                      `((tr ,(map (lambda (token)
                                    (let* ((td-object-raw (string-trim-both token #\"))
                                           (td-object
                                            (if (and (> (string-length td-object-raw) 6)
                                                     (string= "http://" (string-take td-object-raw 7)))
                                                `(a (@ (href ,td-object-raw)) ,(suffix-iri td-object-raw))
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
      (let ((result
             (catch 'system-error
               (lambda _
                 (receive (header port) (sparql-query post-data #:type "text/csv")
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
