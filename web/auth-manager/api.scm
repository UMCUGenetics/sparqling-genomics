;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

(define-module (auth-manager api)
  #:use-module (auth-manager config)
  #:use-module (auth-manager permission-check)
  #:use-module (auth-manager virtuoso)
  #:use-module ((ice-9 popen) #:select (close-pipe))
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (sparql driver)
  #:use-module (sparql parser)
  #:use-module (sparql stream)
  #:use-module (srfi srfi-1)
  #:use-module (logger)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (www db api)
  #:use-module ((www db sessions) #:select (session-cookie-prefix))
  #:use-module (www util)
  #:use-module (system foreign)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web response)
  #:use-module (sxml simple)

  #:export (announce-availibility
            start-server
            request-handler))

(define system-load-average
  (let [(getloadavg-from-c (pointer->procedure int
                            (dynamic-func "getloadavg" (dynamic-link))
                            (list '* int)))]
    (lambda ()
      (let* [(averages (bytevector->pointer
                        (make-bytevector (sizeof double))))
             (success  (getloadavg-from-c averages 1))]
        (if (>= success 0)
            (bytevector-ieee-double-ref
             (pointer->bytevector
              averages (sizeof double)) 0 (native-endianness))
            -1)))))

(define* (api-handler request request-path client-port
                      #:key (username #f) (token #f))
  "Responds to API calls."
  (let [(accept-type  (request-accept request))
        (content-type (request-content-type request))]
    (cond
     [(string= "/api" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
           `((name     . "SPARQLing-genomics Authentication Manager API")
             (homepage . "https://www.sparqling-genomics.org/")))
          (respond-405 client-port '(GET)))]

     [(string= "/api/status" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
                       `((load-average   . ,(system-load-average))
			 (memory-capacity . ,(total-available-memory))
                         (available-cpus . ,(current-processor-count))))
          (respond-405 client-port '(GET)))]

     ;; So we expect all parameters to be sent as url-encoded data,
     ;; and the actual file contents as content of the POST request.  The
     ;; Content-Type should match the actual file content's mime type.
     [(string-prefix? "/api/import-rdf" request-path)
      (cond
       [(not (importing-enabled?))
        (respond-403 client-port accept-type
                     "This node does not accept more data.")]
       [(eq? (request-method request) 'POST)
        (let [(parameters (uri-query (request-uri request)))]
          (if parameters
              (let* [(metadata      (post-data->alist parameters))
                     (graph-uri     (assoc-ref metadata 'graph))
                     (wait-for-more (assoc-ref metadata 'wait-for-more))
                     (upload-dir     (string-append
                                      (www-upload-root) "/" username))
                     (output-port    (begin
                                       (mkdir-p upload-dir)
                                       (mkstemp! (string-append
                                                  upload-dir "/XXXXXX"))))
                     (tmp-filename   (port-filename output-port))
                     (input-port     (request-port request))
                     (bytes-to-fetch (request-content-length request))
                     (bytes-fetched  (sendfile output-port input-port
                                               bytes-to-fetch))]
                (close-port output-port)
                (if (= bytes-fetched bytes-to-fetch)
                    (cond
                     [(equal? (rdf-store-backend) 'virtuoso)
                      (if (not (stage-file tmp-filename graph-uri))
                          (respond-500 client-port accept-type
                                       "Cannot stage the file.")
                          (if wait-for-more
                              (respond-202 client-port)
                              (if (start-bulk-load)
                                  (respond-200 client-port accept-type
                                   `((message . "The file has been imported.")))
                                  (respond-500 client-port accept-type
                                               "Importing failed."))))]
                     [else
                      (respond-500 client-port accept-type
                       "The sg-auth-manager has been misconfigured.")])
                    (respond-500 client-port accept-type
                                 (format #f "Received ~a of ~a bytes."
                                         bytes-fetched
                                         bytes-to-fetch))))
              (respond-400 client-port accept-type
                           "Missing 'graph' parameter.")))]
       [else
        (respond-405 client-port '(POST))])]

     [(or (string-prefix? "/sparql" request-path)
          (string-prefix? "/api/query" request-path))
      (catch #t
        (lambda _
          (unless (eq? (request-method request) 'POST)
            (throw 'not-a-post-request))
          (let* [(parameters (uri-query (request-uri request)))
                 (metadata   (post-data->alist parameters))
                 (hash       (assoc-ref metadata 'project-id))]
            (unless hash (throw 'missing-project-id))
            (let* [(query     (utf8->string (read-request-body request)))
                   (parsed    (parse-query query))]
              (call-with-values
                  (lambda _
                    (if parsed
                        (may-execute? token hash parsed)
                        (values #f "Could not parse the query.")))
                (lambda (allowed? message)
                  (cond
                   [(not allowed?)
                    (log-access username "/api/query" "Denied query request.")
                    (respond-401 client-port accept-type message)]
                   ;; Custom path for Virtuoso because its HTTP implementation
                   ;; falls short, but its ODBC implementation rocks.
                   [(and (eq? (rdf-store-backend) 'virtuoso)
                         (eq? (query-type parsed) 'SELECT))
                    (call-with-values
                        (lambda _ (virtuoso-isql-query query))
                      (lambda (error-port port)
                        (let ((error-file (port-filename error-port)))
                          (if (port-eof? port)
                              (respond-401 client-port accept-type
                                           (get-string-all error-port))
                              (csv-stream port client-port accept-type))
                          (close-pipe port)
                          (close-port error-port)
                          (delete-file error-file))))]
                   [else
                    (call-with-values
                        (lambda _
                          (sparql-query query
                           #:store-backend (rdf-store-backend)
                           #:uri (rdf-store-uri)
                           #:digest-auth
                           (if (and (rdf-store-username)
                                    (rdf-store-password))
                               (string-append
                                (rdf-store-username) ":"
                                (rdf-store-password))
                               #f)))
                      (lambda (header port)
                        (cond
                         [(= (response-code header) 200)
                          (csv-stream port client-port accept-type)]
                         [(= (response-code header) 401)
                          (respond-401 client-port accept-type
                                       "Authentication failed.")]
                         [else
                          (respond-401 client-port accept-type
                                       (get-string-all port))])))]))))))
        (lambda (key . args)
          (match key
            ('not-a-post-request
             (respond-405 client-port '(POST)))
            ('missing-project-id
             (respond-400 client-port accept-type
                          "Missing 'project-id' parameter"))
            (_
             (log-error "/api/query" "Thrown ~a: ~s" key args)
             (respond-500 client-port accept-type
                          "Undefined server error.")))))]

     [else
      (respond-404 client-port accept-type
                   "This method does not exist.")])))

(define (request-handler client-port)
  "The gateway function to the actual API handler."
  (let* [(request            (read-request client-port))
         (uri                (request-uri request))
         (request-path       (uri-path uri))
         (parameters         (uri-query uri))
         (metadata           (post-data->alist parameters))
         (token-parameter    (assoc-ref metadata 'token))
         (accept-type        (request-accept request))
         (headers            (request-headers request))
         (biscuit            (assoc-ref headers 'cookie))]
    ;; There can be multiple cookies on the top-level domain, so we have
    ;; to pick the right one.
    (let* [(cookies-str (cond
                         [biscuit         biscuit]
                         [token-parameter (string-append
                                           (session-cookie-prefix) "="
                                           token-parameter)]
                         [else            #f]))
           (cookies (if (string? cookies-str)
                        (delete #f (map (lambda (cookie)
                                          (if (string-prefix?
                                               (session-cookie-prefix) cookie)
                                              cookie #f))
                                        (map string-trim-both
                                             (string-split cookies-str #\;))))
                        #f))
           (cookie (if (and (list? cookies) (not (null? cookies)))
                       (car cookies)
                       #f))
           (username (token->user cookie))]
      (cond
       ;; Display an HTML page when we can reasonably guess
       ;; that a web browser is doing the request.
       [(and (eq? (request-method request) 'GET)
             (or (equal? '(text/html) accept-type)
                 (member '(text/html) accept-type)))
        (respond-to-client 200 client-port '(text/html)
         (call-with-output-string
           (lambda (port)
             (format port "<!DOCTYPE html>~%")
             (sxml->xml `(html
                          (head (title "sg-auth-manager"))
                          (body
                           (h1 "SPARQLing-genomics authentication manager")
                           (p "You have reached the API of a "
                              (a (@ (href "https://www.sparqling-genomics.org"))
                                 "SPARQLing-genomics") " SPARQL endpoint.")
                           (p "Please see the documentation to learn how to "
                              "use it."))) port))))]
       ;; Deal with unserveable requests.
       [(not (api-serveable-format? accept-type))
        (log-debug "request-handler" "Not a serveable format: ~a" accept-type)
        (respond-406 client-port)]
       ;; Only proceed when the sg-web instance approves.
       [username
        (log-access username request-path)
        (api-handler request request-path client-port
                     #:username username
                     #:token cookie)]
       [else
        (respond-401 client-port (request-accept request)
                     "Please log in.")]))))

(define* (announce-availibility #:key (retries 10))

  (define (wait-and-announce)
    ;; Wait for the sg-auth-manager web server to start.
    (let* ((address (make-socket-address (www-listen-address-family)
                                         (www-listen-address)
                                         (www-listen-port)))
           (s (socket (www-listen-address-family) SOCK_STREAM 0)))
      (while (catch #t
               (lambda _ (connect s address))
               (lambda _ #f))
        (usleep 10))
      (close s))

    ;; Announce availability to the configured sg-web service.
    (catch #t
      (lambda _
        (receive (header port)
          (http-post (string-append (sg-web-uri) "/api/register-connection")
           #:headers    `((content-type . (application/s-expression))
                          (accept       . ((application/s-expression))))
           #:streaming? #t
           #:body       (format #f "~s"
                                `((name          . ,(www-name))
                                  (uri           . ,(self-uri))
                                  (accepts-data? . ,(importing-enabled?)))))
          (cond
           [(= (response-code header) 201)
            (log-debug "sg-auth-manager"
                       "Announcing node availability to ~a succeeded."
                       (sg-web-uri))]

           ;; The ‘sg-web’ service will return a 503 status until it's fully
           ;; operational.  Therefore, when receiving a 503 status, we will
           ;; retry after some time.
           [(= (response-code header) 503)
            (if (> retries 0)
                (begin
                  (log-error "sg-auth-manager"
                             "Retrying announcement in 10 seconds.")
                  (sleep 10)
                  (announce-availibility #:retries (1- retries)))
                (begin
                  (log-error "sg-auth-manager"
                             "It looks like sg-web has a problem; giving up.")
                  (exit 1)))]
           [else
            (log-error "sg-auth-manager"
                       "Registering node ~a failed with status code ~a."
                       (sg-web-uri) (response-code header))
            (log-error "sg-auth-manager" "Please restart sg-auth-manager.")])))
      (lambda (key . args)
        (log-error "sg-auth-manager"
                   "Registering node ~a failed."
                   (sg-web-uri)))))

  (call-with-new-thread wait-and-announce))

(define (start-server request-handler)
  (let* [(family (www-listen-address-family))
         (s      (socket family SOCK_STREAM 0))]
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s family (if (string? (www-listen-address))
                       (inet-pton family (www-listen-address))
                       (www-listen-address))
          (www-listen-port))
    (listen s 128)
    (while #t
      (let* [(client-connection (accept s))
             (client-port       (car client-connection))]

        ;; Each request is handled in a separate thread.
        (call-with-new-thread
         (lambda _
           (sigaction SIGPIPE SIG_IGN)
           (request-handler client-port)
           (close client-port))
         (lambda (key . args)
           (close client-port)))))))
