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

(define-module (www pages project-members)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (sxml simple)

  #:export (page-project-members))

(define* (page-project-members request-path username #:key (post-data ""))
  (let* [(hash        (basename request-path))
         (project     (project-by-hash hash))
         (project-uri (project-id project))
         (is-creator? (project-is-created-by? project-uri username))
         (members     (project-members project-uri))]
    (if (null? members)
        '(p "Could not derive project members from queries.")
        `((table (@ (class "item-table"))
                 (tr (th "Name")
                     (th "# Queries")
                     ,(if is-creator?
                          '(th (@ (style "width: 150pt; text-align: right;"))
                               "Actions")
                          '(th "")))
                 ,(map (lambda (item)
                         `(tr (td ,(assoc-ref item "user"))
                              (td ,(assoc-ref item "queries"))
                              ,(if (and is-creator?
                                        (not (string= (assoc-ref item "user") username)))
                                   `(td (@ (class "button-column left-button-column"))
                                        (form (@ (action ,(string-append "/project-details/" hash))
                                                 (method "post"))
                                              (button (@ (type "submit")
                                                         (class "action-btn remove-btn")
                                                         (name "remove-assigned-member")
                                                         (value ,(assoc-ref item "user")))
                                                      "✖")))
                                   '(td ""))))
                       members))))))
