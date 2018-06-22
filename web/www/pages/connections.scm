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

(define-module (www pages connections)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (page-connections))

;; ----------------------------------------------------------------------------
;; CONNECTIONS-TABLE
;; ----------------------------------------------------------------------------
;;
;; This function queries the internal state database and constructs an SXML
;; equivalent for a HTML table that can be inserted into the page.
;;

(define (connections-table)
  `(table (@ (id "item-table"))
     (tr (th "Connection")
         (th (@ (class "item-table-right")) "Actions"))
     ,(map (lambda (record)
             (let ((name    (connection-name record))
                   (address (connection-address record))
                   (port    (connection-port record)))
               `(tr (td (a (@ (href ,(string-append (www-hostname)) "/" name))
                           ,(string-append name " (" address ":" port ")")))
                    (td (form (@ (action "/connections") (method "post"))
                              (button (@ (type "submit")
                                         (class "action-btn remove-btn")
                                         (name "remove")
                                         (value ,name))
                                      "✖")
                              )))))
           (reverse (all-connections)))))

;; ----------------------------------------------------------------------------
;; PAGE-CONNECTIONS
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-connections request-path #:key (post-data ""))
  (let ((message
         (if (not (string= post-data ""))
             (receive (success? message)
                 (let ((alist (post-data->alist (uri-decode post-data))))
                   (match alist
                     (((address . a) (name . b) (port . c) (username . d) (password . e))
                      (connection-add (alist->connection alist)))
                     (((address . a) (name . b) (port . c))
                      (connection-add (alist->connection alist)))
                     (((remove . a))
                      (connection-remove a))
                     (else     #f)))
               (if success?
                   #f ; No need to display a message.
                   `(div (@ (class "message-box failure")) (p ,message))))
             #f)))
    (page-root-template "Connections" request-path
     `((h2 "Connections"
           ;; The “Add connection” button is on the same line as the title.
           ;; The corresponding CSS makes sure it looks like a button.
           (div (@ (id "add-connection") (class "small-action"))
                (a (@ (href "#")
                      (onclick "javascript:ui_insert_connection_form(); return false;"))
                   "✚")))

       ;; When an action occurred (like “a connection was added”), we display
       ;; the success or error message accordingly.
       ,(if message message '())

       ;; Display the main table.
       ,(connections-table)

       ;; The following javascript code adds the form fields to the table.
       (script "
function ui_insert_connection_form () {
  $('#item-table tbody:last-child').append('"
               (tr (td (@ (colspan "2"))
                       (form (@ (action "/connections") (method "post"))
                         (table (tr (td (input (@ (type "text")
                                                  (id "add-name-field")
                                                  (name "name")
                                                  (placeholder "Name"))))
                                    (td (input (@ (type "text")
                                                  (id "add-address-field")
                                                  (name "address")
                                                  (placeholder "Address"))))
                                    (td (input (@ (type "text")
                                                  (id "add-port-field")
                                                  (name "port")
                                                  (placeholder "Port"))))
                                    (td (@ (class "item-table-right"))
                                        (input (@ (id "add-field-button")
                                                  (type "submit")
                                                  (value "↵"))))))))) "');
  $('#add-field').focus();
  $('#add-connection').remove();
}

$(document).ready(function(){
  $('add-connection').focus();
});
")) #:dependencies '(jquery))))
