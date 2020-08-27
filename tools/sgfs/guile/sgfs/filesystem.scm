(define-module (sgfs filesystem)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (logger)
  #:use-module (sgfs cache)
  #:use-module (web client)
  #:use-module (web response)

  #:export (directory-overview-for-path
            is-directory))

(define (http-error-handler header port)
  (let ((code (response-code header)))
    (cond
     [(= code 401)
      (log-error "http-error-handler"
                 "Authentication failed. Is your token still valid?")]
     [(and (> code 499) (< code 600))
      (log-error "http-error-handler"
                 "The endpoint is experiencing some difficulties.")]
     [else
      (log-error "http-error-handler"
                 "The endpoint returned a ~s error." code)])
    '()))

(define (project-queries endpoint token project-name)
  (let* ((project    (assoc-ref (projects) project-name))
         (project-id (assoc-ref project "id")))
    (if (not project)
        '()
        (receive (header port)
            (http-post (string-append endpoint "/api/queries-by-project")
              #:headers
              `((Cookie       . ,(string-append "SGSession=" token))
                (content-type . (application/s-expression))
                (accept       . ((application/s-expression))))
              #:body (call-with-output-string
                       (lambda (out)
                         (write `((project-id . ,project-id)) out)))
              #:streaming? #t)
          (if (= (response-code header) 200)
              (filter (lambda (item)
                        (not (and (string? item)
                                  (string= item ""))))
                      (map (lambda (query) (assoc-ref query "name"))
                           (read port)))
              (http-error-handler header port))))))

(define (projects-overview endpoint token)
  (receive (header port)
        (http-get (string-append endpoint "/api/projects")
                  #:headers
                  `((Cookie . ,(string-append "SGSession=" token))
                    (accept . ((application/s-expression))))
                  #:streaming? #t)
    (if (= (response-code header) 200)
        (map (lambda (project)
               (add-project-to-cache `(,(assoc-ref project "name") . ,project))
               (assoc-ref project "name"))
             (read port))
        (http-error-handler header port))))

(define (path->components path)
  (if path
      (string-split
       (if (eq? (string-ref path 0) #\/)
           (substring path 1)
           path)
       #\/)
      #f))

(define (is-directory path)
  (let ((components (path->components path)))
    (match components
      (("")
       #t)
      (("Projects")
       #t)
      (("Projects" project-name)
       #t)
      (("Projects" project-name "Queries")
       #t)
      (("Projects" project-name "Queries" filename)
       #f)
      (("Projects" project-name "Results")
       #t)
      (("Projects" project-name "Results" filename)
       #f)
      (("Origins")
       #t)
      (_
       #f))))

(define (directory-overview-for-path endpoint token path)
  (let ((components (path->components path)))
    (match components
      (("")
       '("Projects" "Origins"))

      ;; PROJECT PATTERNS
      ;; ------------------------------------------------------------------
      (("Projects")
       (projects-overview endpoint token))
      (("Projects" project-name)
       '("Queries" "Results"))
      (("Projects" project-name "Queries")
       (project-queries endpoint token project-name))
      (("Projects" project-name "Results")
       '())

      ;; ORIGINS PATTERNS
      ;; ------------------------------------------------------------------

      ;; REST
      ;; ------------------------------------------------------------------
      (_
       '()))))
