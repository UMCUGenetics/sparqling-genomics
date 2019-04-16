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

(define-module (www pages exploratory)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db connections)
  #:use-module (www db overview)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-exploratory))

(define* (page-exploratory request-path username #:key (post-data ""))
  (page-root-template "Exploratory" request-path
   `((h2 "Exploratory"
         (div (@ (id "add-project") (class "small-action action-btn-clear-cache"))
              (a (@ (href "/clear-cache")) "⭯")))
     (p "The exploratory provides an alternative interface to explore the "
        "structure of data available at each connection.  It is optimized "
        "for speed, allowing it to show outdated information.  By using "
        "the ⭯ button, you can request the most recent data.  This may "
        "be a bit slower than showing the outdated information.")
     (form
      (table (@ (id "exploratory-table")
                (class "item-table"))
       (thead
        (tr (th (@ (style "width: 25%; padding-left: 5px")) "Connections")
            (th (@ (style "width: 25%; padding-left: 5px")) "Graphs")
            (th (@ (style "width: 25%; padding-left: 5px")) "Types")
            (th (@ (style "width: 25%; padding-left: 5px")) "Predicates")))
       (tbody
        (tr (td (select (@ (name "connections")
                           (id "connections")
                           (class "connection-selector multiple-selector")
                           (size "100"))))
            (td (select (@ (name "graphs")
                           (id "graphs")
                           (class "graphs-selector multiple-selector")
                           (size "100")
                           (disabled "disabled"))))
            (td (select (@ (name "types")
                           (id "types")
                           (class "types-selector multiple-selector")
                           (size "100")
                           (disabled "disabled"))))
            (td (select (@ (name "predicates")
                           (id "predicates")
                           (class "predicates-selector multiple-selector")
                           (size "100")
                           (disabled "disabled")
                           (onkeydown "keydown(event)")
                           (onchange "updateClipboard(this)")))))
        (tr (td (@ (style "vertical-align: top"))
                (p "A list of connections is stored internally."))
            (td (@ (style "vertical-align: top"))
                (p "To get the graphs, the following query is used:")
                (pre (@ (id "graph-query"))
                     "SELECT DISTINCT ?graph WHERE { GRAPH ?graph { ?s ?p ?o } }"))
            (td (@ (style "vertical-align: top"))
                (p "Types are determined using the following query:")
                (pre (@ (id "type-query"))
                     "SELECT DISTINCT ?type WHERE { ?s rdf:type ?type }"))
            (td (@ (style "vertical-align: top"))
                (p "Predicates are found using the following query:")
                (pre (@ (id "predicate-query"))
                     "SELECT DISTINCT ?predicate WHERE { ?s rdf:type ?type "
                     "; ?predicate ?o . }")))))
      (input (@ (type "text")
                (id "clipboard")
                (onkeyup "keyup(event)")
                ;; This input field is used to store clipboard data.  With the following
                ;; style rules we move it out-of-sight.
                (style "position: absolute; width: 1px; height: 1px; padding:0; left: -9999px;"))))
     (script "
function updateClipboard(selectBox) {
    var clipboard = document.getElementById('clipboard');
    var text = '';
    for (i = 0; i != selectBox.length; i++) {
        if (selectBox.options[i].selected)
          text = selectBox.options[i].value;
    }
    clipboard.value = text;
}

function keydown(e) {
    if(e.keyCode === 17) {
        var clipboard = document.getElementById('clipboard');
        clipboard.select();
    }
}

function keyup(e) {
    if(e.keyCode === 17) {
        var selectBox = document.getElementById('predicates');
        selectBox.focus();
    }
}

$(document).ready(function(){
  $.get('/connections.json', function(data){
    var connections = JSON.parse(data);
    connections.map(function (connection){
      $('#connections').append('" (option (@ (value "'+ connection +'"))
                                          "'+ connection +'") "');
    });
  });

  var st = jQuery.parseHTML('<')[0].nodeValue;
  var gt = jQuery.parseHTML('>')[0].nodeValue;

  $('#connections').on('change', function(){
    $('#graphs').find('option').remove();
    $('#graphs').prop('disabled', true);
    $('#types').find('option').remove()
    $('#types').prop('disabled', true);
    $('#predicates').find('option').remove()
    $('#predicates').prop('disabled', true);

    var connection = $('#connections option:selected' ).val();
    post_data = { connection: connection };
    $.post('/graphs.json', JSON.stringify(post_data), function(data){
      var graphs = JSON.parse(data);
      $('#graphs').prop('disabled', false);
      graphs.map(function (graph){
        $('#graphs').append('" (option (@ (value "'+ graph +'"))
                                            "'+ graph +'") "');
      });
    });
  });

  $('#graphs').on('change', function(){
    $('#types').find('option').remove();
    $('#types').prop('disabled', true);
    $('#predicates').find('option').remove()
    $('#predicates').prop('disabled', true);
    var connection = $('#connections option:selected' ).val();
    var graph      = $('#graphs option:selected' ).val();
    post_data = { connection: connection, graph: graph };
    $.post('/types.json', JSON.stringify(post_data), function(data){
      var types = JSON.parse(data);
      $('#types').prop('disabled', false);
      types.map(function (type){
        $('#types').append('" (option (@ (value "'+ type +'"))
                                      "'+ type +'") "');
      });
      $('#type-query').text(
       'PREFIX rdf: '+ st +'http://www.w3.org/1999/02/22-rdf-syntax-ns#'+ gt +'\\n' +
       '\\n'                                                          +
       'SELECT DISTINCT ?type\\n'                                     +
       'WHERE {\\n'                                                   +
       '  GRAPH '+ st + graph + gt + ' {\\n'                          +
       '    ?s rdf:type ?type .\\n'                                   +
       '  }\\n'                                                       +
       '}');
    });
  });

  $('#types').on('change', function(){
    $('#predicates').find('option').remove();
    $('#predicates').prop('disabled', true);
    var connection = $('#connections option:selected' ).val();
    var graph      = $('#graphs option:selected' ).val();
    var type       = $('#types option:selected' ).val();
    post_data = { connection: connection, graph: graph, type: type };
    $.post('/predicates.json', JSON.stringify(post_data), function(data){
      var predicates = JSON.parse(data);
      $('#predicates').prop('disabled', false);
      predicates.map(function (predicate){
        $('#predicates').append('" (option (@ (value "'+ predicate +'"))
                                      "'+ predicate +'") "');
      });

      $('#predicate-query').text(
       'PREFIX rdf: '+ st +'http://www.w3.org/1999/02/22-rdf-syntax-ns#'+ gt +'\\n' +
       '\\n'                                                          +
       'SELECT DISTINCT ?predicate\\n'                                +
       'WHERE {\\n'                                                   +
       '  GRAPH '+ st + graph + gt + ' {\\n'                          +
       '    ?s rdf:type   '+ st + type + gt +' ;\\n'                  +
       '       ?predicate ?o .\\n'                                    +
       '  }\\n'                                                       +
       '}');

    });
  });

});"))
   #:dependencies '(jquery)))
