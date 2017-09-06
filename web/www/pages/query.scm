(define-module (www pages query)
  #:use-module (www pages)
  #:export (page-query))

(define* (page-query request-path #:key (post-data ""))
  (page-root-template "sparqling-svs" request-path
   `((h2 "Query the database")
     (h3 "Query editor")
     (p "Use " (strong "Ctrl + Enter") " to execute the query.")
     (div (@ (id "editor")) "PREFIX grch38: <http://rdf.ebi.ac.uk/resource/ensembl/90/homo_sapiens/GRCh38/>
PREFIX faldo: <http://biohackathon.org/resource/faldo#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX : <http://localhost:5000/cth/>

SELECT DISTINCT ?variant ?svtype ?geneName ?ensemblGeneId ?location {
    ?variant a :StructuralVariant .
    ?variant :genome_position ?p .
    ?variant :filter ?filter .
    ?variant :type ?svtype .
    ?p :chromosome ?chromosome .
    ?p :position ?position .
    ?location faldo:reference grch38:1 .
    ?location faldo:begin ?begin .
    ?location faldo:end ?end .
    ?begin faldo:position ?beginPosition .
    ?end faldo:position ?endPosition .
    ?ensemblGeneId a ?type ;
          rdfs:label ?label ;
          dc:description ?desc ;
          dc:identifier ?id ;
          faldo:location ?location .
      ?ensemblGeneId rdfs:label ?geneName .
    FILTER (?chromosome = \"1\")
    FILTER (?position > (?beginPosition) && ?position < (?endPosition))
}
LIMIT 10
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
      $('#editor').after(function(){ return '"
      (div (@ (class "query-data-loader"))
           (div (@ (class "title")) "Loading data ...")
           (div (@ (class "content")) "Please wait until the results appears."))
      "' });

        /* Remove the previous query results. */
      $('.query-error').remove();
      $('#query-results').remove();
      $('#query-output').remove();
      $('#query-output_wrapper').remove();

      $.post('/query-response', editor.getValue(), function (data){

        /*  Insert the results HTML table into the page. */
        $('#editor').after(data);
        $('.query-data-loader').remove();

        /* Detect an error response. */
        if ($('.query-error').length == 0) {
          $('#editor').after(function(){ return '" (h3 (@ (id "query-results")) "Query results") "' });

          /* Initialize DataTables. */
          $('#query-output').addClass('display');
          var dt = $('#query-output').DataTable({ sDom: 'lrtip' });
          dt.draw();
        }
      });
      }, readOnly: true
    });
});
"))
   #:dependencies '(ace jquery datatables)))
