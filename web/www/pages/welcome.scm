(define-module (www pages welcome)
  #:use-module (www pages)
  #:export (page-welcome))

(define* (page-welcome request-path #:key (post-data ""))
  (page-root-template "sparqling-svs" request-path
   `((h2 "Query the database")
     (h3 "Query editor")
     (p "Use " (strong "Ctrl + Enter") " to execute the query.")
     (div (@ (id "editor")) "PREFIX : <http://localhost:8890/TestGraph/>

SELECT ?variant ?chromosome ?position ?filter
WHERE {
  ?variant a :SNPVariant .
  ?variant :genome_position ?p .
  ?variant :filter ?filter .
  ?p :chromosome ?chromosome .
  ?p :position ?position .

  FILTER (?chromosome = \"1\")
  FILTER (?position > 1000000)
  FILTER (?position < 9000000)
}
LIMIT 500
")
     (script "
$(document).ready(function(){
  var editor = ace.edit('editor');
  var session = editor.getSession();
  editor.setTheme('ace/theme/github');
  editor.setShowPrintMargin(false);
  session.setMode('ace/mode/sparql');
  session.setTabSize(2);

  /* Set the editor to the maximum height within the content area. */
  //var page_height = $(document).height();
  //var content_pos = $('#content').position();
  //var editor_pos = $('#editor').position();
  //var editor_height = page_height - 85 - editor_pos.top;
  //$('#wrapper').height(page_height);
  //$('#content').height(page_height - content_pos.top - 100);
  //$('#editor').height(editor_height);

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
      $.post('/query-response', editor.getValue(), function (data){

        /* Remove the previous query results. */
        $('#query-results').remove();
        $('#query-output').remove();
        $('#query-output_wrapper').remove();

        /*  Insert the results HTML table into the page. */
        $('#editor').after(data);
        $('#editor').after(function(){ return '" (h3 (@ (id "query-results")) "Query results") "' });

        /* Initialize DataTables. */
        $('#query-output').addClass('display');
        var dt = $('#query-output').DataTable({ sDom: 'lrtip' });
        dt.draw();
      });
      }, readOnly: true
    });
});
"))
   #:dependencies '(ace jquery datatables)))
