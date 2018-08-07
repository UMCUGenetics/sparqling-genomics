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

(define-module (www pages edit-project)
  #:use-module (www pages)
  #:use-module (www pages projects)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db projects)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (page-edit-project))

;; ----------------------------------------------------------------------------
;; PAGE-EDIT-PROJECT
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-edit-project request-path #:key (post-data ""))
  (let* ((name       (last (string-split request-path #\/)))
         (message
          (if (not (string= post-data ""))
              (receive (success? message)
                  (let ((alist (post-data->alist (uri-decode post-data))))
                    (match alist
                      (((name . a) (samples . b))
                       (project-edit (alist->project alist)))
                      (else
                       (values #f "Invalid form data."))))
                (if success?
                    #f ; No need to display a message.
                    `(div (@ (class "message-box failure")) (p ,message))))
              #f))
         (project (project-by-name name))
         (title (string-append "Edit “" name "”")))
    (page-root-template title request-path
     `((h2 ,title)
       ;; When an action occurred (like “the project was modified”), we
       ;; display the success or error message accordingly.
       ,(if message message '())

       ;; Display the main table.
       (form (@ (action ,(string-append "/edit-project/" name))
                (method "post"))
             (input (@ (type "hidden")
                       (name "name")
                       (value ,(project-name project))))
             (table (tr
                     (td "Name")
                     (td (@ (style "width: 90%"))
                         (input (@ (type "text")
                                      (id "add-name-field")
                                      (name "name-disabled")
                                      (value ,(project-name project))
                                      (placeholder "Name")
                                      (disabled "disabled")))))
                    (tr
                     (td "Samples")
                     (td (input (@ (type "text")
                                      (id "add-samples-field")
                                      (name "samples")
                                      (value ,(project-samples project))
                                      (placeholder "Wait for autocompletion..")))))
                    (tr
                     (td "")
                     (td (@ (class "item-table-right"))
                            (input (@ (id "edit-button")
                                      (type "submit")
                                      (value "Save modifications"))))))))
     #:dependencies '(jquery))))
