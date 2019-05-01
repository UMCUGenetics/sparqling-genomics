;;; Copyright © 2016, 2017, 2018  Roel Janssen <roel@gnu.org>
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

(define-module (www pages query)
  #:use-module (www pages)
  #:use-module (www db connections)
  #:use-module (www db queries)
  #:use-module (www db projects)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (srfi srfi-1)
  #:export (page-query))

(define* (page-query request-path username #:key (post-data #f))
  (page-root-template username "Query" request-path
   `((h2 "Query the database")
     ,(let* ((connections (all-connections username #:filter connection-name))
             (alist       (if post-data (post-data->alist post-data) '()))
             (query       (assoc-ref alist 'query))
             (project     (active-project-for-user username))
             (endpoint    (if (assoc-ref alist 'endpoint)
                              (assoc-ref alist 'endpoint)
                              "")))
        ;; Handle removal instructions.
        (when (assoc-ref alist 'remove)
          (query-remove (assoc-ref alist 'remove) username))
        (cond
         [(null? connections)
          ;; Before we can query, there must be a connection that we can query on.
          ;; The best we can do is refer to creating a connection at this point.
          `((h3 "Create a connection")
            (p "Please " (a (@ (href "/connections")) "create a connection") " first."))]
         [(null? project)
          `((h3 "Set active project")
            (p "Please set one of your " (a (@ (href "/projects")) "projects") " to active first."))]
         [else
          ;; Queries are executed on a connection, so we must give users the choice
          ;; to select the appropriate connection.
          `((h3 "Select a connection")
              (select (@ (id "connection"))
                      ,(map (lambda (connection)
                              `(option (@ (value ,connection)
                                          ,(if (string= endpoint connection)
                                               `(selected "selected")
                                               '(class "not-selected")))
                                       ,connection))
                            connections))

              (h3 "Query editor")
              (p "Use " (strong "Ctrl + Enter") " to execute the query. ("
                 (strong "Cmd + Enter") " for the unfortunate MacOS users.)")
              (div (@ (id "editor"))
                   ,(if query
                        query
                        (format #f "~a~%SELECT ?s ?p ?o { ?s ?p ?o }~%LIMIT 100~%"
                                default-prefixes)))

              `((h3 "History")

                (p "The table below contains queries that were previously "
                   "executed. For compactness, all " (code "PREFIX") " "
                   "declarations and empty lines are not shown.")

                (div (@ (class "history-data-loader"))
                     (div (@ (class "title")) "Loading history ...")
                     (div (@ (class "content")) "Please wait for the results to appear.")))
              (script "
$(document).ready(function(){

  $.get('/query-history', function (data){
      $('.history-data-loader').after(data);
      $('.history-data-loader').remove();
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
  session.setMode('ace/mode/sparql');
  session.setTabSize(2);

  /* Add keybindings for copying the text and for running the query. */
  editor.commands.addCommand({
    name: 'copyCommand',
    bindKey: {win: 'Ctrl-C',  mac: 'Command-C'},
    exec: function(editor) {
      $('#content').after('" (textarea (@ (id "copyText"))) "');
      var temp = document.getElementById('copyText');
      temp.value = editor.getSelectedText();
      temp.select();
      document.execCommand('copy');
      temp.remove();
      $('.ace_text-input').focus();
      }, readOnly: true
    });

  editor.commands.addCommand({
    name: 'executeQueryCommand',
    bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
    exec: function(editor) {
      $('#editor').after(function(){ return '"
      (div (@ (class "query-data-loader"))
           (div (@ (class "title")) "Loading data ...")
           (div (@ (class "content")) "Please wait for the results to appear."))
      "' });

      /* Remove the previous query results. */
      $('.query-error').remove();
      $('#query-results').remove();
      $('#query-output').remove();
      $('#query-output_wrapper').remove();

      post_data = { query: editor.getValue(), connection: $('#connection').val() };
      $.post('/query-response', JSON.stringify(post_data), function (data){

        /*  Insert the results HTML table into the page. */
        $('#editor').after(data);
        $('.query-data-loader').remove();
        $('#note-five-thousand').remove();

        /* Detect an error response. */
        if ($('.query-error').length == 0) {
          $('#editor').after(function(){ return '"
      ((h3 (@ (id "query-results")) "Query results")
       (p (@ (id "note-five-thousand"))
          (strong "Note:") " Query results are limited to a maximum of 5000 rows.  Programmatic access does not have this limitation.")) "' });

          /* Initialize DataTables. */
          $('#query-output').addClass('display');
          var dt = $('#query-output').DataTable({ sDom: 'lrtip' });
          dt.draw();

          $.get('/query-history', function (data){
            $('#query-history-table').replaceWith(data);
          });

        }
      });
      }, readOnly: true
    });
});
"))])))
   #:dependencies '(ace jquery datatables)))
