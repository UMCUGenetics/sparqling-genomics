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

(define-module (www config-reader)
  #:use-module (ldap authenticate)
  #:use-module (logger)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (www config)
  #:use-module (www db connections)
  #:export (read-configuration-from-file))

(define (read-configuration-from-file filename)
  (log-debug "read-configuration-from-file" "Reading ~s." filename)
  (catch #t
    (lambda _
      (let* ((sxml-data (call-with-input-file filename
                          (lambda (port)
                            (xml->sxml port #:trim-whitespace? #t))))
             (config (assoc-ref sxml-data 'web-interface)))

        ;; Handle options
        ;; --------------------------------------------------------------------
        (let [(fork?             (assoc-ref config 'fork))
              (address           (assoc-ref config 'bind-address))
              (port              (assoc-ref config 'port))
              (api               (assoc-ref config 'api))
              (authentication    (assoc-ref config 'authentication))
              (sys-connection    (assoc-ref config 'system-connection))]
          (when (and fork? (> (string->number (car fork?)) 0))
            (set-fork-on-startup! #t))
          (when port
            (set-www-listen-port! (string->number (car port))))
          (when address
            (set-www-listen-address! (car address)))
          (when api
            (set-api-enabled! (string= (car api) "1")))
          (when authentication
            (cond
             ;; LDAP is preferred over single-user authentication.
             [(assoc-ref authentication 'ldap)
              (if (not (ldap-is-available?))
                  (begin
                    (display "Warning: Your configuration specifies an LDAP ")
                    (display "connection, but LDAP is unavailable.  Please ")
                    (display "enable LDAP-support when building ")
                    (display "sparqling-genomics.")
                    (newline))
                  (let ((ldap (assoc-ref authentication 'ldap)))
                    (set-ldap-uri! (car (assoc-ref ldap 'uri)))
                    (set-ldap-organizational-unit!
                     (car (assoc-ref ldap 'organizational-unit)))
                    (set-ldap-domain! (car (assoc-ref ldap 'domain)))
                    (set-ldap-enabled! #t)))]
             ;; Single-user configuration is an alternative to LDAP auth.
             [(assoc-ref authentication 'single-user)
              (let ((user (assoc-ref authentication 'single-user)))
                (if (not user)
                    (begin
                      (display "Warning: Your configuration specifies ")
                      (display "single-user authentication, but your ")
                      (display "configuration is incomplete.")
                      (newline))
                    (if (or (null? (assoc-ref user 'username))
                            (null? (assoc-ref user 'password)))
                        (begin
                          (display "An empty username or password is not ")
                          (display "allowed.")
                          (newline))
                        (let ((username (car (assoc-ref user 'username)))
                              (password (car (assoc-ref user 'password))))
                          (begin
                            (set-authentication-username! username)
                            (set-authentication-password! password)
                            (set-authentication-enabled! #t))))))]))
          (if sys-connection
              (let [(uri         (assoc-ref sys-connection 'uri))
                    (backend     (assoc-ref sys-connection 'backend))
                    (username    (assoc-ref sys-connection 'username))
                    (password    (assoc-ref sys-connection 'password))]
                (cond
                 [(not uri)       (throw 'invalid-system-connection "No URI specified.")]
                 [(null? uri)     (throw 'invalid-system-connection "The URI may not be empty.")]
                 [(not backend)   (throw 'invalid-system-connection "No backend specified.")]
                 [(null? backend) (throw 'invalid-system-connection "Invalid backend specified.")]
                 [(not (member (string->symbol (car backend))
                               (sparql-available-backends)))
                  (throw 'invalid-system-connection "Invalid backend specified.")]
                 [else
                  (set-system-connection! (alist->connection
                                           `((name     . "system-connection")
                                             (uri      . ,(car uri))
                                             (username . ,(if (or (not username)
                                                                  (null? username))
                                                              ""
                                                              (car username)))
                                             (password . ,(if (or (not password)
                                                                  (null? password))
                                                              ""
                                                              (car password)))
                                             (backend  . ,(car backend)))))]))
              (throw 'invalid-system-connection "Missing 'system-connection'."))
          #t)))
    (lambda (key . args)
      (cond
       [eq? key 'invalid-system-connection
            (begin
              (display "Error: There was a problem with the ")
              (display "'system-connection' property:")
              (newline)
              (display (car args))
              (newline)
              (exit 1)
              #f)]
       [else
        (format #t "Error: Couldn't read configuration file (~a: ~a).~%"
                key args)
        #f]))))
