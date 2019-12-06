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

(define-module (www requests-api)
  #:use-module (ice-9 receive)
  #:use-module (ldap authenticate)
  #:use-module (logger)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www config)
  #:use-module (www db api)
  #:use-module (www db queries)
  #:use-module (www db connections)
  #:use-module (www db projects)
  #:use-module (www db sessions)
  #:use-module (www pages)
  #:use-module (www util)

  #:export (request-api-handler
            authenticate-user))

(define (authenticate-user data)
  "This function returns a user session on success or #f on failure."

  (if (or (and (ldap-enabled?)
               (may-access?
                (ldap-uri) (ldap-common-name) (ldap-organizational-unit)
                (ldap-domain)
                (assoc-ref data 'username)
                (assoc-ref data 'password)))
          (and (not (ldap-enabled?))
               (not (null? (local-users)))
               (any (lambda (x) x)
                    (map (lambda (user)
                           (and (string= (car user) (assoc-ref data 'username))
                                (string= (cadr user) (string->sha256sum
                                                      (assoc-ref data 'password)))))
                         (local-users)))))
      (let ((session (session-by-username (assoc-ref data 'username))))
        (unless session
          (set! session (alist->session
                         `((username . ,(assoc-ref data 'username))
                           (token    . ""))))
          (session-add session))
        session)
      #f))

(define* (request-api-handler request request-body request-path
                              client-port #:key (username #f))
  (let [(accept-type  (request-accept request))
        (content-type (request-content-type request))]
    (cond

     ;; LOGIN
     ;; ---------------------------------------------------------------------
     [(string= "/api/login" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data    (api-request-data->alist
                           content-type (utf8->string request-body)))
                 (session (authenticate-user data))]
            (if session
                (begin
                  (log-debug "api-login"
                             "User ~s logged in to the API." username)
                  (respond-200-with-cookie client-port
                    (string-append "SGSession=" (session-token session))))
                (respond-401 client-port accept-type
                             "Invalid username or password.")))
          (respond-405 client-port '(POST)))]

     ;; The remainder of API calls is expected to return data.  For these
     ;; calls, the format in which to send data is important, and therefore
     ;; when the client does not provide an 'Accept' header, or does not
     ;; request a supported format, we do not need to process the API call
     ;; further, because the response will always be a 406.
     [(not (api-serveable-format? accept-type))
      (respond-406 client-port)]

     ;; PROJECTS
     ;; ---------------------------------------------------------------------
     [(string= "/api/projects" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type (projects-by-user username))
          (respond-405 client-port '(GET)))]

     ;; ACTIVE-PROJECT
     ;; ---------------------------------------------------------------------
     [(string= "/api/active-project" request-path)
      (if (eq? (request-method request) 'GET)
          (let [(project (active-project-for-user username))]
            (if (null? project)
                (respond-200 client-port accept-type '())
                (respond-200 client-port accept-type
                             (project-by-id project))))
          (respond-405 client-port '(GET)))]

     ;; ASSIGN-GRAPH
     ;; ---------------------------------------------------------------------
     [(string= "/api/assign-graph" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (api-request-data->alist
                               content-type (utf8->string request-body)))
                 (project-uri (assoc-ref data 'project-uri))
                 (graph-uri   (assoc-ref data 'graph-uri))]
            (if (project-has-member? project-uri username)
                (if (project-assign-graph! project-uri graph-uri username)
                    (respond-201 client-port)
                    (respond-500 client-port accept-type "Not OK"))
                (respond-401 client-port accept-type "Not allowed.")))
          (respond-405 client-port '(POST)))]

     ;; UNASSIGN-GRAPH
     ;; ---------------------------------------------------------------------
     [(string= "/api/unassign-graph" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (api-request-data->alist
                               content-type (utf8->string request-body)))
                 (project-uri (assoc-ref data 'project-uri))
                 (graph-uri   (assoc-ref data 'graph-uri))]
            (if (project-has-member? project-uri username)
                (if (project-forget-graph! project-uri graph-uri)
                    (respond-204 client-port)
                    (respond-500 client-port accept-type "Not OK"))
                (respond-401 client-port accept-type "Not allowed.")))
          (respond-405 client-port '(POST)))]

     ;; ADD-PROJECT
     ;; ---------------------------------------------------------------------
     [(string= "/api/add-project" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (api-request-data->alist
                               content-type (utf8->string request-body)))
                 (name        (assoc-ref data 'name))]
            (receive (state message)
                (project-add name username)
              (if state
                  (respond-201 client-port)
                  (respond-403 client-port accept-type message))))
          (respond-405 client-port '(POST)))]

     ;; SET-AS-ACTIVE-PROJECT
     ;; ---------------------------------------------------------------------
     [(string= "/api/set-as-active-project" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (api-request-data->alist
                               content-type (utf8->string request-body)))
                 (project-uri (assoc-ref data 'project-uri))]
            (if (project-has-member? project-uri username)
                (if (set-active-project-for-user! username project-uri)
                    (respond-204 client-port)
                    (respond-500 client-port accept-type "Not OK"))
                (respond-403 client-port accept-type
                             "You are not a member of this project.")))
          (respond-405 client-port '(POST)))]

     ;; REMOVE-PROJECT
     ;; ---------------------------------------------------------------------
     [(string= "/api/remove-project" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(data        (api-request-data->alist
                               content-type (utf8->string request-body)))
                 (project-uri (assoc-ref data 'project-uri))]
             (if (project-has-member? project-uri username)
                (receive (state message)
                    (project-remove project-uri username)
                  (if state
                      (respond-204 client-port)
                      (respond-403 client-port accept-type message)))
                (respond-403 client-port accept-type
                             "You are not a member of this project.")))
          (respond-405 client-port '(POST)))]

     ;; QUERIES
     ;; ---------------------------------------------------------------------
     [(string= "/api/queries" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type (queries-by-username username))
          (respond-405 client-port '(GET)))]

     ;; CONNECTIONS
     ;; ---------------------------------------------------------------------
     [(string= "/api/connections" request-path)
      (if (eq? (request-method request) 'GET)
          (respond-200 client-port accept-type
                       (map connection->alist (all-connections username)))
          (respond-405 client-port '(GET)))]

     ;; ADD-CONNECTION
     ;; ---------------------------------------------------------------------
     [(string= "/api/add-connection" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(connections (all-connections username))
                 (data        (api-request-data->alist
                               content-type (utf8->string request-body)))
                 (record      (alist->connection data))]
            (receive (state message)
                (connection-add record connections username)
              (if state
                  (respond-201 client-port)
                  (respond-403 client-port accept-type message))))
          (respond-405 client-port '(POST)))]

     ;; REMOVE-CONNECTION
     ;; ---------------------------------------------------------------------
     [(string= "/api/remove-connection" request-path)
      (if (eq? (request-method request) 'POST)
          (let* [(connections (all-connections username))
                 (data        (api-request-data->alist
                               content-type (utf8->string request-body)))
                 (name        (assoc-ref data 'name))]
            (receive (state message)
                (connection-remove name connections username)
              (if state
                  (respond-204 client-port)
                  (respond-403 client-port accept-type message))))
          (respond-405 client-port '(POST)))]

     [else
      (respond-404 client-port accept-type "This method does not exist.")])))
