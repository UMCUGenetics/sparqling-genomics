;;; Copyright © 2016, 2017, 2018, 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www requests)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (ldap authenticate)
  #:use-module (logger)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www config)
  #:use-module (www db cache)
  #:use-module (www db connections)
  #:use-module (www db portal)
  #:use-module (www db projects)
  #:use-module (www db prompt)
  #:use-module (www db queries)
  #:use-module (www db sessions)
  #:use-module (www pages edit-connection)
  #:use-module (www pages error)
  #:use-module (www pages project-assigned-graphs)
  #:use-module (www pages project-dependent-graphs)
  #:use-module (www pages project-details)
  #:use-module (www pages project-members)
  #:use-module (www pages project-queries)
  #:use-module (www pages welcome)
  #:use-module (www pages)
  #:use-module (www util)

  #:export (request-handler
            start-server))

;; ----------------------------------------------------------------------------
;; HANDLERS
;; ----------------------------------------------------------------------------
;;
;; The way a request is handled varies upon the nature of the request.  It can
;; be as simple as serving a pre-existing file, or as complex as finding a
;; Scheme module to use for handling the request.
;;
;; In this section, the different handlers are implemented.
;;

(define (request-file-handler path client-port)
  "This handler takes data from a file and sends that as a response."

  (define (response-content-type path)
    "This function returns the content type of a file based on its extension."
    (let ((extension (substring path (1+ (string-rindex path #\.)))))
      (cond [(string= extension "css")  '(text/css)]
            [(string= extension "js")   '(application/javascript)]
            [(string= extension "json") '(application/javascript)]
            [(string= extension "html") '(text/html)]
            [(string= extension "n3")  '(text/plain)]
            [(string= extension "png")  '(image/png)]
            [(string= extension "svg")  '(image/svg+xml)]
            [(string= extension "ico")  '(image/x-icon)]
            [(string= extension "pdf")  '(application/pdf)]
            [(string= extension "ttf")  '(application/font-sfnt)]
            [(#t '(text/plain))])))

  (let ((full-path (string-append (www-root) "/" path)))
    (if (not (file-exists? full-path))
        (respond-to-client 404 client-port '(text/html)
          (with-output-to-string
            (lambda _ (sxml->xml (page-error-404 path)))))
        (let* [(file-stat (stat full-path))
               (bytes     (stat:size file-stat))]
          (write-response
           (build-response
            #:code 200
            #:headers `((content-type . ,(response-content-type full-path))
                        (content-length . ,bytes)))
           client-port)
          (call-with-input-file full-path
            (lambda (input-port)
              (sendfile client-port input-port bytes)))))))

(define* (request-scheme-page-handler request request-body request-path
                                      client-port #:key (username #f))

  (define (module-path prefix elements)
    "Returns the module path so it can be loaded."
    (append prefix (map string->symbol elements)))

  (define (resolve-module-function request-path)
    "Return FUNCTION from MODULE."
    (let* ((module (resolve-module (module-path '(www pages)
                     (string-split request-path #\/)) #:ensure #f))
           (page-symbol (symbol-append 'page-
                         (string->symbol
                          (string-replace-occurrence request-path #\/ #\-)))))
      ;; Return #f unless the 'page-symbol' exists in 'module',
      ;; in which case we return that.
      (if module
          (catch #t
            (lambda _ (module-ref module page-symbol))
            (lambda (key . args) #f))
          #f)))

  ;; Return-type handlers.
  ;; --------------------------------------------------------------------------
  (cond
   ;; The “/” page is special, because we re-route it to “welcome”.
   [(not (string-is-longer-than request-path 2))
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (format port "<!DOCTYPE html>~%")
          (sxml->xml (page-welcome "/" username) port))))]

   ;; The POST request of the login page is special, because it must set
   ;; a Set-Cookie HTTP header.  This is something out of the control of
   ;; the normal page functions.
   [(and (string-prefix? "/login" request-path)
         (eq? (request-method request) 'POST))
    (let [(data (post-data->alist (utf8->string request-body)))]
      (if (or (and (ldap-enabled?)
                   (may-access?
                     (ldap-uri) (ldap-organizational-unit) (ldap-domain)
                     (assoc-ref data 'username)
                     (assoc-ref data 'password)))
              (and (not (ldap-enabled?))
                   (authentication-enabled?)
                   (string= (authentication-username)
                            (assoc-ref data 'username))
                   (string= (authentication-password)
                            (string->sha256sum (assoc-ref data 'password)))))
          (let ((session (session-by-username (assoc-ref data 'username))))
            (unless session
              (set! session (alist->session
                             `((username . ,(assoc-ref data 'username))
                               (token    . ""))))
              (session-add session))
            ;; Redirect to the “welcome” page.
            (write-response (build-response
                             #:code 303
                             #:headers
                             `((Location   . "/")
                               (Set-Cookie . ,(string-append
                                               "SGSession="
                                               (session-token session)))))
                            client-port))
          (respond-to-client 200 client-port '(text/html)
            (call-with-output-string
              (lambda (port)
                (set-port-encoding! port "utf8")
                (let* ((page-function (resolve-module-function "login"))
                       (sxml-tree     (page-function request-path
                                                     #:post-data
                                                     (utf8->string request-body))))
                  (catch 'wrong-type-arg
                    (lambda _
                      (when (eq? (car (car sxml-tree)) 'html)
                        (format port "<!DOCTYPE html>~%")))
                    (lambda (key . args) #f))
                  (sxml->xml sxml-tree port)))))))]

   ;; The regular login page is special because the username
   ;; isn't known at this point.
   [(and (string-prefix? "/login" request-path)
         (eq? (request-method request) 'GET))
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (let* ((page-function (resolve-module-function "login"))
                 (sxml-tree     (page-function request-path)))
            (catch 'wrong-type-arg
              (lambda _
                (when (eq? (car (car sxml-tree)) 'html)
                  (format port "<!DOCTYPE html>~%")))
              (lambda (key . args) #f))
            (sxml->xml sxml-tree port)))))]

   [(string-prefix? "/logout" request-path)
    (write-response
      (build-response
       #:code 303
       #:headers `((Location . "/")
                   (Set-Cookie  . ,(string-append
                                    "SGSession=deleted; expires=Thu,"
                                    " Jan 01 1970 00:00:00 UTC;"))))
      client-port)]

   ;; ;; When the URI begins with “/project-queries/”, use the project-queries
   ;; ;; page to construct a suitable output.
   [(string-prefix? "/project-queries" request-path)
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (sxml->xml
           (page-project-queries request-path username #:post-data '())
           port))))]

   [(string-prefix? "/query-response" request-path)
    (let [(response-mime-type (if (string-suffix? ".json" request-path)
                                  '(application/javascript)
                                  '(text/html)))]
      (respond-to-client 200 client-port response-mime-type
      ;; TODO: Let query-response stream the results, rather than
      ;; building it up in-memory and send it in a single large response.
      (call-with-output-string
        (lambda (port)
          (let* ((path          "query-response")
                 (page-function (resolve-module-function path)))
            (when page-function
              (if (eq? (request-method request) 'POST)
                  ((page-function request-path username
                                  #:post-data (utf8->string request-body)
                                  #:return-type response-mime-type) port)
                  (page-function request-path username))))))))]

   ;; When the “file extension” of the request indicates JSON, treat the
   ;; returned format as ‘application/javascript’.
   [(string-suffix? ".json" request-path)
    (respond-to-client 200 client-port '(application/javascript)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (let* ((request-path (basename request-path ".json"))
                 (page-function (resolve-module-function request-path)))
            (if page-function
                (if (eq? (request-method request) 'POST)
                    (put-string port
                                (page-function
                                 request-path username
                                 #:type 'json
                                 #:post-data (utf8->string request-body)))
                    (put-string port (page-function request-path username
                                                    #:type 'json)))
                (put-string port "[]"))))))]

   ;; When the URI begins with “/edit-connection/”, use the edit-connection
   ;; page.
   [(string-prefix? "/edit-connection" request-path)
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (format port "<!DOCTYPE html>~%")
          (sxml->xml (if (eq? (request-method request) 'POST)
                         (page-edit-connection request-path username
                            #:post-data (utf8->string request-body))
                         (page-edit-connection request-path username))
                     port))))]

   ;; When the URI begins with “/project-details/”, use the project-details
   ;; page.
   [(string-prefix? "/project-details" request-path)
    (catch #t
      (lambda _
        (let* [(hash    (last (string-split request-path #\/)))
               (project (project-by-hash hash))]
          (if (project-has-member? (project-id project) username)
              (respond-to-client 200 client-port '(text/html)
                (call-with-output-string
                 (lambda (port)
                   (set-port-encoding! port "utf8")
                   (format port "<!DOCTYPE html>~%")
                   (sxml->xml (if (eq? (request-method request) 'POST)
                                  (page-project-details request-path username
                                   #:post-data (utf8->string request-body))
                                  (page-project-details request-path username))
                              port))))
              (throw 'no-access))))
      (lambda (key . args)
        (respond-to-client 404 client-port '(text/html)
          (with-output-to-string
           (lambda _ (sxml->xml (page-error-404 request-path)))))))]

   [(string-prefix? "/project-members" request-path)
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (format port "<!DOCTYPE html>~%")
          (sxml->xml (if (eq? (request-method request) 'POST)
                         (page-project-members request-path username
                                               #:post-data (utf8->string request-body))
                         (page-project-members request-path username))
                     port))))]

   [(string-prefix? "/project-dependent-graphs" request-path)
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (format port "<!DOCTYPE html>~%")
          (sxml->xml (if (eq? (request-method request) 'POST)
                         (page-project-dependent-graphs request-path username
                                                        #:post-data (utf8->string request-body))
                         (page-project-dependent-graphs request-path username))
                     port))))]

   [(string-prefix? "/project-assigned-graphs" request-path)
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          (format port "<!DOCTYPE html>~%")
          (sxml->xml (if (eq? (request-method request) 'POST)
                         (page-project-assigned-graphs request-path username
                                                       #:post-data (utf8->string request-body))
                         (page-project-assigned-graphs request-path username))
                     port))))]

   [(string-prefix? "/clear-exploratory-cache" request-path)
    (cache-clear username)
    (write-response (build-response
                     #:code 303
                     #:headers `((Location   . "/exploratory")))
                    client-port)]

   [(string-prefix? "/clear-overview-cache" request-path)
    (cache-clear username)
    (write-response (build-response
                     #:code 303
                     #:headers `((Location   . "/")))
                    client-port)]

   ;; For “/query-history-clean”, we must call a database function and
   ;; redirect to “/query”.
   [(string-prefix? "/query-history-clear" request-path)
    (query-remove-unmarked username)
    (write-response (build-response
                     #:code 303
                     #:headers `((Location   . "/query")))
                    client-port)]

   [(string-prefix? "/prompt-session-clear" request-path)
    (prompt-clear-triplets username)
    (write-response (build-response
                     #:code 303
                     #:headers `((Location   . "/prompt")))
                    client-port)]

   [(string-prefix? "/prompt-session-save" request-path)
    (catch #t
      (lambda _
        (let* [(post-data (post-data->alist (uri-decode
                                             (utf8->string request-body))))
               (graph (assoc-ref post-data 'select-graph))]
          (prompt-save-session username graph)))
      (lambda (key . args) #f))

    (write-response (build-response
                     #:code 303
                     #:headers `((Location   . "/prompt")))
                    client-port)]

   [(string-prefix? "/prompt-remove-triplet" request-path)
    (catch #t
      (lambda _
        (let* [(json-data (json-string->scm (utf8->string request-body)))
               (subject   (hash-ref json-data "subject"))
               (predicate (hash-ref json-data "predicate"))
               (object    (hash-ref json-data "object"))]
          (prompt-remove-triplet username subject predicate object)))
      (lambda (key . args) #f))
    (write-response (build-response
                     #:code 303
                     #:headers `((Location   . "/prompt")))
                    client-port)]

   [(string-prefix? "/portal-filter-query" request-path)
    (catch #t
      (lambda _
        (let* [(json-data (json-string->scm (utf8->string request-body)))
               (response-data (filtered-datasets-query json-data))]
          (respond-to-client 200 client-port '(text/plain) response-data)))
      (lambda (key . args)
        ;; 204 No Content
        (write-response (build-response #:code 204) client-port)))]

   ;; All other requests can be handled as HTML responses.
   [#t
    (respond-to-client 200 client-port '(text/html)
      (call-with-output-string
        (lambda (port)
          (set-port-encoding! port "utf8")
          ;; Use block-buffering for a higher I/O throughput, but don't
          ;; set it on Guile 2.0, because setting it on string ports
          ;; is not needed/supported.
          (unless (string= (effective-version) "2.0")
            (setvbuf port 'block 4096))
          (let* ((path          (substring request-path 1))
                 (page-function (resolve-module-function path))
                 (sxml-tree     (if page-function
                                    (if (eq? (request-method request) 'POST)
                                        (page-function request-path username
                                                       #:post-data
                                                       (utf8->string request-body))
                                        (page-function request-path username))
                                    (page-ontology-or-error-404
                                     request-path))))
            (catch 'wrong-type-arg
              (lambda _
                (when (eq? (car (car sxml-tree)) 'html)
                  (format port "<!DOCTYPE html>~%")))
              (lambda (key . args) #f))
            (sxml->xml sxml-tree port)))))]))


;; ----------------------------------------------------------------------------
;; ROUTING & HANDLERS
;; ----------------------------------------------------------------------------
;;
;; Requests can have different handlers.
;; * Static objects (images, stylesheet, javascript files) have their own
;;   handler.
;; * Package pages are generated dynamically, so they have their own handler.
;; * The 'regular' Scheme pages have their own handler that resolves the
;;   module dynamically.
;;
;; Feel free to add your own handler whenever that is necessary.
;;

(define (request-handler client-port)
  (let* [(request      (read-request client-port))
         (request-path (uri-path (request-uri request)))
         (request-body (read-request-body request))
         (headers      (request-headers request))]
    ;; There can be multiple cookies on the top-level domain, so we have
    ;; to pick the right one;  the one with session name 'SGSession'.
    (let* ((cookies-str (assoc-ref headers 'cookie))
           (cookies (if (string? cookies-str)
                        (delete #f (map (lambda (cookie)
                                          (if (string-prefix? "SGSession=" cookie)
                                              cookie #f))
                                        (map string-trim-both
                                             (string-split cookies-str #\;))))
                        #f))
           (token (if (and (list? cookies) (not (null? cookies)))
                      (car cookies)
                      #f)))
      (cond
       ;; Static resources are served using the ‘request-file-handler’.
       ;; ----------------------------------------------------------------------
       [(string-prefix? "/static/" request-path)
        (request-file-handler request-path client-port)]

       ;; Authentication is required for almost all pages.
       ;; ----------------------------------------------------------------------
       [(and (string? token)
             ;; The token starts with 'SGSession=', we have to strip that
             ;; off to get the actual token.
             (is-valid-session-token? (substring token 10)))
        (let* ((real-token  (substring token 10))
               (username    (session-username (session-by-token real-token))))
          (log-access username request-path)
          (request-scheme-page-handler
           request request-body request-path client-port #:username username))]

       ;; The following pages may be access without logging in.
       ;; ----------------------------------------------------------------------
       [(or (string= "/login" request-path)
            (string= "/portal" request-path))
        (request-scheme-page-handler
         request request-body request-path client-port)]

       ;; When not authenticated, redirect from the main page to the portal.
       ;; ----------------------------------------------------------------------
       [(string= "/" request-path)
        (write-response (build-response
                         #:code 303
                         #:headers '((Location . "/portal")))
                        client-port)]

       [else
        (write-response (build-response
                         #:code 303
                         #:headers '((Location . "/login")))
                        client-port)]))))

(define (start-server request-handler)
  (let ((s (socket PF_INET SOCK_STREAM 0)))
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)

    (bind s AF_INET (if (string? (www-listen-address))
                        (inet-pton AF_INET (www-listen-address))
                        (www-listen-address))
          (www-listen-port))

    (listen s 10)
    (while #t
      (let* [(client-connection (accept s))
             (client-details    (cdr client-connection))
             (client-port       (car client-connection))]

        ;; Each request is handled in a separate thread.
        (call-with-new-thread
         (lambda _
           (request-handler client-port)
           (close client-port))
         (lambda (key . args)
           (close client-port)))))))
