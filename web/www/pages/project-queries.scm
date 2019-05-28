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

(define-module (www pages project-queries)
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

  #:export (page-project-queries))

(define (strip-prefix-lines query)
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (line)
                  (unless (or (string-prefix? "prefix" line)
                              (string-prefix? "PREFIX" line)
                              (string= line ""))
                    (format port "~a~%" line)))
                (string-split query #\newline)))))

(define* (page-project-queries request-path username #:key (post-data ""))
  (let* [(hash        (basename request-path))
         (project     (project-by-hash hash))
         (queries     (queries-by-project (project-id project)))]
    (if (null? queries)
        '(p "There are no queries associated with this project.")
        `((table (@ (class "item-table"))
                 (tr (th "Query")
                     (th "Connection")
                     (th "Duration (in seconds)")
                     (th "Executed by")
                     (th (@ (style "width: 150pt; text-align: right;")
                            (colspan "5"))
                         "Actions"))
                 ,(map (lambda (query)
                         `(tr (td (pre ,(strip-prefix-lines (query-content query))))
                              (td ,(query-endpoint query))
                              (td ,(if (query-execution-time query)
                                       (query-execution-time query)
                                       "Unknown"))
                              (td ,(basename (query-username query)))
                              (td "")
                              (td (@ (class "button-column left-button-column"))
                                  ,(if (string= (basename (query-username query)) username)
                                       `(form (@ (action "/query") (method "post"))
                                              (div (@ (class "mark-box-wrapper"))
                                                   (input (@ (type "checkbox")
                                                             (id ,(string-append
                                                                   "mark-"
                                                                   (basename (query-id query))))
                                                             (class "mark-box")
                                                             ,(if (query-marked? query)
                                                                  '(checked "checked")
                                                                  '(name "favorite"))
                                                             (onchange ,(string-append
                                                                         "toggle_marker('"
                                                                         (basename (query-id query))
                                                                         "', '"
                                                                         (query-id query)
                                                                         "'); return false"))
                                                             (value ,(query-id query))))
                                                   (label (@ (for ,(string-append
                                                                    "mark-"
                                                                    (basename (query-id query))))))))
                                       '()))
                              (td (@ (class "button-column left-button-column"))
                                  ,(if (string= (basename (query-username query)) username)
                                       `(form (@ (action "/query") (method "post"))
                                              (button (@ (type "submit")
                                                         (class "action-btn remove-btn")
                                                         (name "remove")
                                                         (value ,(query-id query)))
                                                      "✖"))
                                       '()))
                              (td (@ (class "button-column left-button-column"))
                                  (form (@ (action "/query") (method "post"))
                                        (input (@ (type "hidden")
                                                  (name "endpoint")
                                                  (value ,(query-endpoint query))))
                                        (button (@ (type "submit")
                                                   (class "action-btn insert-btn")
                                                   (name "query")
                                                   (value ,(query-content query)))
                                                "⤴")))
                              (td (@ (class "button-column right-button-column"))
                              (form (@ (action "/plottable-query") (method "post"))
                                    (input (@ (type "hidden")
                                              (name "endpoint")
                                              (value ,(query-endpoint query))))
                                    (button (@ (type "submit")
                                               (class "action-btn plot-btn")
                                               (name "query")
                                               (value ,(query-content query)))
                                            "📊")))))
                       queries))
          (script "
function toggle_marker (id, queryId) {
  var state = document.getElementById('mark-'+ id).checked;
  var post_data = { 'query-id': queryId, 'state': state };
  $.post('/query-history-mark.json', JSON.stringify(post_data), function(data){
    var message = JSON.parse(data);
    document.getElementById('mark-'+ id).checked = message[0].state;
  });
}
")))))
