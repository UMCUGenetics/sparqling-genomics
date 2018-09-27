;;; Copyright © 2018  Roel Janssen <roel@gnu.org>
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

(define-module (www pages login)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db connections)
  #:use-module (www db overview)
  #:use-module (www db sessions)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ldap authenticate)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-login))

(define* (page-login request-path #:key (post-data ""))
  (if (string= post-data "")
      ;; Handle non-POST requests.
      (page-empty-template "Log in" request-path
       `((div (@ (id "login-wrapper"))
              (form (@ (action "/login") (method "POST"))
                    (input (@ (type "text")
                              (class "login-field")
                              (name "username")
                              (placeholder "Username")))
                    (input (@ (type "password")
                              (class "login-field")
                              (name "password")
                              (placeholder "Password")))
                    (input (@ (class "login-field")
                              (type "submit")
                              (value "Log in")))))))
      ;; Handle POST-requests.
      ;; When we get here, the login as failed, because otherwise the
      ;; page handler would've redirected us already.
      (let ((data (post-data->alist post-data)))
        (page-empty-template "Log in" request-path
         `((div (@ (id "login-wrapper"))
                (div (@ (class "message-box failure"))
                     (p "Authentication failed."))
                (form (@ (action "/login") (method "POST"))
                      (input (@ (type "text")
                                (class "login-field")
                                (name "username")
                                (placeholder "Username")))
                      (input (@ (type "password")
                                (class "login-field")
                                (name "password")
                                (placeholder "Password")))
                      (input (@ (class "login-field")
                                    (type "submit")
                                    (value "Log in"))))))))))
