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

(define-module (test endpoint)
  #:use-module (ice-9 receive)
  #:use-module (rnrs io ports)
  #:use-module (sparql parser)
  #:use-module (srfi srfi-1)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (www util)

  #:export (run-endpoint-test))

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

;; The main entry point for this module.
;; ----------------------------------------------------------------------------
(define (run-endpoint-test endpoint-uri token)
  (let ((cookie          (string-append "SGSession=" token))
        (endpoint        (lambda (path)
                           (string-append endpoint-uri path)))
        (connection-name (random-ascii-string 32))
        (connections     '())
        (projects        '())
        (queries         '())
        (new-project     '())
        (session         '()))

    ;; List connections
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-get (endpoint "/api/connections")
                  #:headers
                  `((Cookie . ,cookie)
                    (accept . ((application/s-expression))))
                  #:streaming? #t)

      (cond
       [(= (response-code header) 200)
        (set! connections (read port))
        (success "~a connection~a are available."
                 (length connections)
                 (if (> (length connections) 1) "s" ""))]
       [else
        (error "Call to /api/connections failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; List projects
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-get (endpoint "/api/projects")
                  #:headers
                  `((Cookie . ,cookie)
                    (accept . ((application/s-expression))))
                  #:streaming? #t)

      (cond
       [(= (response-code header) 200)
        (set! projects (read port))
        (success "~a project~a are available."
                 (length projects)
                 (if (> (length projects) 1) "s" ""))]
       [else
        (error "Call to /api/projects failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; List queries
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-get (endpoint "/api/queries")
                  #:headers
                  `((Cookie       . ,cookie)
                    (accept       . ((application/s-expression))))
                   #:streaming? #t)
      (cond
       [(= (response-code header) 200)
        (set! queries (read port))
        (success "~a ~a are available."
                 (length queries)
                 (if (= (length queries) 1) "query" "queries"))]
       [else
        (error "Call to /api/queries failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; Add connection
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-post (endpoint "/api/add-connection")
                   #:headers
                   `((Cookie       . ,cookie)
                     (accept       . ((application/s-expression)))
                     (content-type . (application/s-expression)))
                   #:streaming? #t
                   #:body
                   (call-with-output-string
                     (lambda (out)
                       (write `((name . ,connection-name)
                                (uri  . "https://non-existent.tld:9999/sparql")
                                (backend . "4store")) out))))

      (cond
       [(= (response-code header) 201)
        (success "Connection ~s was added." connection-name)]
       [else
        (error "Call to /api/add-connection failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; Remove connection
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-post (endpoint "/api/remove-connection")
                   #:headers
                   `((Cookie       . ,cookie)
                     (accept       . ((application/s-expression)))
                     (content-type . (application/s-expression)))
                   #:streaming? #t
                   #:body
                   (call-with-output-string
                     (lambda (out)
                       (write `((name . ,connection-name)) out))))

      (cond
       [(= (response-code header) 204)
        (success "Connection ~s was removed." connection-name)]
       [else
        (error "Call to /api/remove-connection failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; Add project
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-post (endpoint "/api/add-project")
                   #:headers
                   `((Cookie       . ,cookie)
                     (accept       . ((application/s-expression)))
                     (content-type . (application/s-expression)))
                   #:streaming? #t
                   #:body
                   (call-with-output-string
                     (lambda (out)
                       (write `((name . ,(random-ascii-string 128))) out))))
      (cond
       [(= (response-code header) 200)
        (set! new-project (read port))
        (success "Project ~s has been created."
                 (assoc-ref new-project 'project-id))]
       [else
        (error "Call to /api/add-project failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; Remove project
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-post (endpoint "/api/remove-project")
                   #:headers
                   `((Cookie       . ,cookie)
                     (accept       . ((application/s-expression)))
                     (content-type . (application/s-expression)))
                   #:streaming? #t
                   #:body
                   (call-with-output-string
                     (lambda (out)
                       (let ((hash (assoc-ref new-project 'project-id)))
                         (write `((project-hash . ,hash)) out)))))
      (cond
       [(= (response-code header) 204)
        (success "Project ~s has been removed."
                 (assoc-ref new-project 'project-id))
        (set! new-project '())]
       [else
        (error "Call to /api/add-project failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; Create a session token
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-post (endpoint "/api/new-session-token")
                   #:headers
                   `((Cookie       . ,cookie)
                     (accept       . ((application/s-expression)))
                     (content-type . (application/s-expression)))
                   #:streaming? #t
                   #:body
                   (call-with-output-string
                     (lambda (out)
                       (write `((session-name . ,(random-ascii-string 32)))
                              out))))
      (cond
       [(= (response-code header) 200)
        (set! session (read port))
        (success "Created session with token ~s."
                 (assoc-ref session 'token))]
       [else
        (error "Call to /api/new-session-token failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))

    ;; Create a session token
    ;; ------------------------------------------------------------------------
    (receive (header port)
        (http-post (endpoint "/api/remove-session")
                   #:headers
                   `((Cookie       . ,cookie)
                     (accept       . ((application/s-expression)))
                     (content-type . (application/s-expression)))
                   #:streaming? #t
                   #:body
                   (call-with-output-string
                     (lambda (out)
                       (write `((token . ,(assoc-ref session 'token))) out))))
      (cond
       [(= (response-code header) 200)
        (success "Removed session with token ~s."
                 (assoc-ref session 'token))
        (set! session '())]
       [else
        (error "Call to /api/remove-session failed with ~a:~%~a"
               (response-code header)
               (get-string-all port))]))))
