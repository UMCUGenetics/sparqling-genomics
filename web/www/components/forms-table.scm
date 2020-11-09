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

(define-module (www components forms-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (json)
  #:use-module (sparql driver)
  #:use-module (srfi srfi-1)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www util)
  #:export (forms-table))

;; ----------------------------------------------------------------------------
;; FORMS-TABLE
;; ----------------------------------------------------------------------------

(define (forms-table username)
  `(table (@ (id "forms-table")
             (class "item-table"))
          (tr (th (@ (class "item-table-left")) "Form"
                  (div (@ (id "add-form")
                          (class "small-action"))
                       (a (@ (href "/create-new-form")) "✚")))
         (th (@ (style "min-width: 30pt")
                (colspan "2")) "Actions"))
     ,(map (lambda (record)
             (let [(name        (form-name       record))
                   (public-url  (form-public-url record))
                   (edit-url    (form-edit-url   record))]
               `(tr (td (a (@ (href ,edit-url))
                           ,(string-append name)))
                    (td (@ (class "button-column"))
                        ,(if is-default?
                             '()
                             `(form (@ (action "/remove-form")
                                       (method "post"))
                                    (button (@ (type "submit")
                                               (class "action-btn remove-btn")
                                               (name "name")
                                               (value ,name))
                                            "✖")))))))
           (connections-by-user username))))

(define* (rdf-stores-component redirect-url username #:optional (message #f))
  (let* [(connections (connections-by-user username))]
    `( ;; When an action failed (like “a connection couldn't be added”), we display
       ;; the error message accordingly.
       ,(if message message '())

       ;; Display the main table.
       ,(connections-table username)

       ;; The following javascript code adds the form fields to the table.
       (script "
function ui_insert_form_form () {
  $('#forms-table tbody:last-child').append('"
               (tr (td (@ (colspan "2"))
                       (form (@ (action "/add-form")
                                (method "post"))
                         (table
                          (tr (td (input (@ (type "text")
                                            (id "add-name-field")
                                            (name "name")
                                            (placeholder "Name"))))
                              (td (input (@ (type "text")
                                            (id "add-uri-field")
                                            (name "uri")
                                            (placeholder "http://example.org:8890/sparql"))))
                              (td (input (@ (type "text")
                                            (id "add-username-field")
                                            (name "username")
                                            (placeholder "Username (optional)"))))
                              (td (input (@ (type "password")
                                            (id "add-password-field")
                                            (name "password")
                                            (placeholder "Password (optional)"))))
                              (td (select
                                   (@ (name "backend"))
                                   ,(map (lambda (backend)
                                           `(option (@ (value ,backend)) ,backend))
                                         (map symbol->string (sparql-available-backends)))))
                              (td (@ (style "width: 32px"))
                                  (input (@ (id "add-field-button")
                                            (type "submit")
                                            (value "↵"))))))))) "');
  $('#add-field').focus();
  $('#add-form').remove();
}

$(document).ready(function(){
  $('add-form').focus();
});
"))))
