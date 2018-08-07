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

(define-module (www pages projects)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db projects)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (page-projects
            projects-table))

;; ----------------------------------------------------------------------------
;; PROJECTS-TABLE
;; ----------------------------------------------------------------------------
;;
;; This function queries the internal state database and constructs an SXML
;; equivalent for a HTML table that can be inserted into the page.
;;

(define (projects-table)
  `(table (@ (id "item-table"))
     (tr (th "Project")
         (th (@ (class "item-table-right")) "Actions"))
     ,(map (lambda (record)
             (let ((name    (project-name    record))
                   (samples (project-samples record)))
               `(tr (td (a (@ (href ,(string-append "/edit-project/" name)))
                           ,name))
                    (td (form (@ (action "/projects") (method "post"))
                              (button (@ (type "submit")
                                         (class "action-btn remove-btn")
                                         (name "remove")
                                         (value ,name))
                                      "✖")
                              )))))
           (reverse (all-projects)))))

;; ----------------------------------------------------------------------------
;; PAGE-PROJECTS
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-projects request-path #:key (post-data ""))
  (let ((message
         (if (not (string= post-data ""))
             (receive (success? message)
                 (let ((alist (post-data->alist (uri-decode post-data))))
                   (match alist
                     (((name . a) (samples . b))
                      (project-add (alist->project alist)))
                     (((remove . a))
                      (project-remove a))
                     (else     #f)))
               (if success?
                   #f ; No need to display a message.
                   `(div (@ (class "message-box failure")) (p ,message))))
             #f)))
    (page-root-template "Projects" request-path
     `((h2 "Projects"
           ;; The “Add project” button is on the same line as the title.
           ;; The corresponding CSS makes sure it looks like a button.
           (div (@ (id "add-project") (class "small-action"))
                (a (@ (href "#")
                      (onclick "javascript:ui_insert_project_form(); return false;"))
                   "✚")))

       ;; When an action occurred (like “a project was added”), we display
       ;; the success or error message accordingly.
       ,(if message message '())

       ;; Display the main table.
       ,(projects-table)

       ;; The following javascript code adds the form fields to the table.
       (script "
function ui_insert_project_form () {
  $('#item-table tbody:last-child').append('"
               (tr (td (@ (colspan "2"))
                       (form (@ (action "/projects") (method "post"))
                         (table (tr (td (input (@ (type "text")
                                                  (id "add-name-field")
                                                  (name "name")
                                                  (placeholder "Name"))))
                                    (td (input (@ (type "text")
                                                  (id "add-samples-field")
                                                  (name "samples")
                                                  (placeholder "Wait for autocompletion.."))))
                                    (td (@ (class "item-table-right"))
                                        (input (@ (id "add-field-button")
                                                  (type "submit")
                                                  (value "↵"))))))))) "');
  $('#add-field').focus();
  $('#add-project').remove();
}

$(document).ready(function(){
  $('add-project').focus();
});
")) #:dependencies '(jquery))))
