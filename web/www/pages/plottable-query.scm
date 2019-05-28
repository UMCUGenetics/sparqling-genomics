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

(define-module (www pages plottable-query)
  #:use-module (www pages)
  #:use-module (www db connections)
  #:use-module (www db queries)
  #:use-module (www db projects)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (srfi srfi-1)
  #:export (page-plottable-query))

(define* (page-plottable-query request-path username #:key (post-data #f))
  (page-root-template username "Plottable Query" request-path
   `((h2 "Query visualisation")
     ,(let* ((connections (all-connections username #:filter connection-name))
             (alist       (if post-data (post-data->alist post-data) '()))
             (query       (assoc-ref alist 'query))
             (project     (active-project-for-user username))
             (endpoint    (if (assoc-ref alist 'endpoint)
                              (assoc-ref alist 'endpoint)
                              '())))
        (cond
         [(or (null? query)
              (null? endpoint))
          `((h3 "Run a query")
            (p "Please " (a (@ (href "/query")) "run a query") " first."))]
         [(null? project)
          `((h3 "Set active project")
            (p "Please set one of your " (a (@ (href "/projects")) "projects")
               " to active first."))]
         [else
          ;; Queries are executed on a connection, so we must give users the choice
          ;; to select the appropriate connection.
          `((h3 "Plot preview")
            (div (@ (id "plot-wrapper"))
                 (svg (@ (id "plot"))))

            (h3 "Query")
            (p (strong "Endpoint: ") ,endpoint)
            (form (input (@ (type "hidden")
                            (id "connection")
                            (name "connection")
                            (value ,endpoint))))
            (div (@ (id "editor")
                    (disabled "disabled"))
                 ,query)

            (script "
$(document).ready(function(){

  post_data = { query: $('#editor').text(), connection: $('#connection').val() };
  $.post('/query-response.json', JSON.stringify(post_data), function (data){
    plot_data('#plot', data);
  });

  var editor = ace.edit('editor');
  var session = editor.getSession();
  editor.setTheme('ace/theme/crimson_editor');
  editor.setShowPrintMargin(false);
  editor.setAutoScrollEditorIntoView(true);
  editor.setOptions({ maxLines: 120,
                      minLines: 2,
                      enableBasicAutocompletion: true,
                      enableLiveAutocompletion: true });
  editor.setReadOnly(true);
  session.setMode('ace/mode/sparql');
  session.setTabSize(2);
});
"))])))
   #:dependencies '(ace jquery d3 plottable-query)))
