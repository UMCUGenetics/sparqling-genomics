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

(define-module (www pages prompt)
  #:use-module (www pages)
  #:use-module (www db connections)
  #:use-module (www db queries)
  #:use-module (www db projects)
  #:use-module (www db prompt)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:export (page-prompt))

(define (page-prompt-layout request-path username insert-successfully?)
  (page-root-template username "Prompt" request-path
    (let [(connections (all-connections username #:filter connection-name))]
      `((h2 "Prompt")

        ,(if (not insert-successfully?)
             `(div (@ (class "message-box failure"))
                   (p "Inserting the triplet failed."))
             '())

        ,(if (not (default-connection username))
             `(p "Please " (a (@ (href "/connections"))
                              "set a connection as default")
                 " first.")
             `((form
                (div (@ (id "prompt-wrapper"))
                     (table
                      (tr (td (@ (class "prompt")) "#")
                          (td (@ (class "input-field"))
                              (input (@ (type "text")
                                        (id "prompt-field")
                                        (name "prompt-field")
                                        (autocomplete "off")
                                        (class "search-field")
                                        (placeholder "Triplets work best"))))))))

               (h3 "Session"
                   (div (@ (id "clear-prompt-session")
                           (class "small-action action-btn-remove"))
                        (a (@ (href "/prompt-session-clear"))
                           "✖")))
               (p "")
               (table (@ (id "prompt-session-table")
                         (class "item-table"))
                      (tr (th "Subject")
                          (th "Predicate")
                          (th "Object")))
               (p "Store the session to the following graph:")
               (form (@ (action "/prompt-session-save") (method "post"))
                ,(let [(graphs (active-writable-graphs-for-user username))]
                   (if (null? graphs)
                       `(p "To save this session, unlock at least one graph "
                           "in your active project.")
                       `(select (@ (id "select-graph")
                                   (name "select-graph"))
                         ,(map (lambda (graph)
                                 `(option (@ (value ,(assoc-ref graph "graph")))
                                          ,(assoc-ref graph "graph")))
                               graphs))))
                (input (@ (id "add-field-button")
                          (style "margin-left: 5pt;")
                          (type "submit")
                          (value "💾"))))))
        (script "
$(document).ready(function(){
  enable_prompt('#prompt-field');
  $('#prompt-field').focus();

  $.get('/prompt-session-table', function(data){
    $('#prompt-session-table').replaceWith(data);
  });
});")))
    #:dependencies '(jquery autocomplete prompt)))

(define* (page-prompt request-path username #:key (post-data #f))
  (if post-data
    (let* [(data       (json-string->scm post-data))
           (subject    (hash-ref data "subject"))
           (predicate  (hash-ref data "predicate"))
           (object     (hash-ref data "object"))]
      (page-prompt-layout request-path username
        (prompt-insert-triplet username subject predicate object)))
    (page-prompt-layout request-path username #t)))
