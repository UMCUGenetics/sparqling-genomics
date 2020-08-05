;;; Copyright © 2018, 2019  Roel Janssen <roel@gnu.org>
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
  #:use-module (www config)
  #:use-module (www pages)
  #:export (page-login))

(define* (page-login request-path #:key (post-data ""))
  (let [(form `(form (@ (action "/login") (method "POST"))
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
                               (value "Log in")))))]
    (cond
     [(orcid-enabled?)
      (page-empty-template "Log in" request-path
       `((div (@ (id "login-wrapper"))
              (p "Log in with:")
              (div (@ (class "large-orcid-btn"))
                   (a (@ (href ,(string-append
                                 (orcid-endpoint) "/authorize"
                                 "?client_id=" (orcid-client-id)
                                 "&response_type=code"
                                 "&scope=/authenticate"
                                 "&redirect_uri=" (orcid-redirect-uri))))
                      (img (@ (src "/static/images/orcid-logo.png")
                              (alt "ORCID"))))))))]
     [(string= post-data "")
      ;; Handle non-POST requests.
      (page-empty-template "Log in" request-path
       `((div (@ (id "login-wrapper")) ,form)))]

     [else
      ;; Handle POST-requests.
      ;; When we get here, the login as failed, because otherwise the
      ;; page handler would've redirected us already.
      (page-empty-template "Log in" request-path
       `((div (@ (id "login-wrapper"))
              (div (@ (class "message-box failure"))
                   (p "Authentication failed."))
              ,form)))])))
