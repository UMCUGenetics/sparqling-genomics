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

(define-module (www pages project-details)
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
  #:export (page-project-details))

;; ----------------------------------------------------------------------------
;; PAGE-PROJECT-DETAILS
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-project-details request-path username #:key (post-data ""))

  (let* [(hash    (last (string-split request-path #\/)))
         (project (project-by-hash hash))
         (title   (project-name project))
         (message
          (if (not (string= post-data ""))
              (receive (success? message)
                  (let ((alist (post-data->alist (uri-decode post-data))))
                    (match alist
                      [(('assign-graph . a))
                       (project-assign-graph! (project-id project) a username)]
                      [(('assign-member . a))
                       (project-assign-member! (project-id project) a username)]
                      [(('remove-assigned-graph . a))
                       (project-forget-graph! (project-id project) a)]
                      [(('lock-assigned-graph . a))
                       (project-lock-assigned-graph! (project-id project) a)]
                      [(('unlock-assigned-graph . a))
                       (project-unlock-assigned-graph! (project-id project) a)]
                      [(('remove-assigned-member . a))
                       (project-forget-member! (project-id project) a)]
                      [else (values #t "")]))
                (if success?
                    #f ; No need to display a message.
                    `(div (@ (class "message-box failure")) (p ,message))))
              #f))]
    (page-root-template title request-path
     `((h2 ,title)
       ;; When an action occurred (like “the project was modified”), we
       ;; display the success or error message accordingly.
       ,(if message message '())

       (h3 "Members"
           (div (@ (id "add-assigned-member")
                   (class "smaller-action"))
                (a (@ (href "#")
                      (onclick "javascript:ui_insert_assigned_member_form(); return false;"))
                   "✚")))
       (div (@ (id "project-members")) "")

       (h3 "Assigned graphs"
           (div (@ (id "add-assigned-graph")
                   (class "smaller-action"))
                (a (@ (href "#")
                      (onclick "javascript:ui_insert_assigned_graph_form(); return false;"))
                   "✚")))
       (p "Members of this project have access to the following graphs.")
       (div (@ (id "project-assigned-graphs")) "")

       (h3 "Inferred associated graphs")
       (p "The following graphs are used in one of the queries associated with this project.")
       (div (@ (id "project-dependent-graphs")) "")

       (h3 "Queries")
       (div (@ (id "project-queries")) "")

       (script "
function ui_insert_assigned_graph_form () {
  $('#assigned-graphs tbody:last-child').append('"
         (tr (td (@ (colspan "4"))
                 (form (@ (action "/project-details/" ,hash)
                          (method "post"))
                       (table (tr (td (@ (style "width: 100%"))
                                      (input (@ (type "text")
                                                (id "add-uri-field")
                                                (name "assign-graph")
                                                (placeholder "URI"))))
                                  (td (@ (class "item-table-right"))
                                      (input (@ (id "add-field-button")
                                                (type "submit")
                                                (value "↵"))))))))) "');
  $('#add-field').focus();
  $('#add-assigned-graph').remove();
}

function ui_insert_assigned_member_form () {
  $('#project-members tbody:last-child').append('"
         (tr (td (@ (colspan "4"))
                 (form (@ (action "/project-details/" ,hash)
                          (method "post"))
                       (table (tr (td (@ (style "width: 100%"))
                                      (input (@ (type "text")
                                                (id "add-username")
                                                (name "assign-member")
                                                (placeholder "Username"))))
                                  (td (@ (class "item-table-right"))
                                      (input (@ (id "add-field-button")
                                                (type "submit")
                                                (value "↵"))))))))) "');
  $('#add-field').focus();
  $('#add-assigned-member').remove();
}

$(document).ready(function(){
  $.get('/project-queries/" ,hash "', function(data) {
    $('#project-queries').replaceWith('" (div (@ (id "project-queries")) "'+ data +'") "');
  });

  $.get('/project-members/" ,hash "', function(data) {
    $('#project-members').replaceWith('" (div (@ (id "project-members")) "'+ data +'") "');
  });

  $.get('/project-assigned-graphs/" ,hash "', function(data) {
    $('#project-assigned-graphs').replaceWith('" (div (@ (id "project-assigned-graphs")) "'+ data +'") "');
  });

  $.get('/project-dependent-graphs/" ,hash "', function(data) {
    $('#project-dependent-graphs').replaceWith('" (div (@ (id "project-dependent-graphs")) "'+ data +'") "');
  });
});
"))
     #:dependencies '(jquery))))
