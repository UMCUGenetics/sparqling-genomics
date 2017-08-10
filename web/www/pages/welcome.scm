(define-module (www pages welcome)
  #:use-module (www pages)
  #:export (page-welcome))

(define (page-welcome request-path)
  (page-root-template "sparqling-svs" request-path
   `((h2 "Query the database")
     (p "Use " (strong "Ctrl + Enter") " to execute the query.")
     (div (@ (id "editor")) "PREFIX : <http://localhost:8890/TestGraph>

SELECT ?variant
WHERE {
  ?variant a :Variant
}
ORDER BY ?variant")
     (script "
$(document).ready(function(){
  var editor = ace.edit('editor');
  var session = editor.getSession();
  editor.setTheme('ace/theme/github');
  editor.setShowPrintMargin(false);
  session.setMode('ace/mode/sparql');
  session.setTabSize(2);

  /* Set the editor to the maximum height within the content area. */
  var page_height = $(document).height();
  var content_pos = $('#content').position();
  var editor_pos = $('#editor').position();
  var editor_height = page_height - 85 - editor_pos.top;
  $('#wrapper').height(page_height);
  $('#content').height(page_height - content_pos.top - 100);
  $('#editor').height(editor_height);

  /* Add keybindings for copying the text and for running the query. */
  editor.commands.addCommand({
    name: 'copyCommand',
    bindKey: {win: 'Ctrl-C',  mac: 'Command-C'},
    exec: function(editor) {
      $('#content').after('" (textarea (@ (id "copyText"))) "');
      var temp = document.getElementById('copyText');
      temp.value = editor.getValue();
      temp.select();
      document.execCommand('copy');
      temp.remove();
      }, readOnly: true
    });

  editor.commands.addCommand({
    name: 'executeQueryCommand',
    bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
    exec: function(editor) {
      alert('Sorry.  The actual query functionality is still missing..');
      }, readOnly: true
    });
});
"))
   #:dependencies '(ace jquery)))
