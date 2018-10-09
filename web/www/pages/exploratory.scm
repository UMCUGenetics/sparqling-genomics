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
  (let ((number-of-endpoints (length (all-connections username))))
    (page-root-template "Exploratory" request-path
     `((h2 "Exploratory")
       (p "The exploratory provides a structured way of finding predicates.")
       (p "")
       (form
        (table
         (thead
          (tr (th (@ (style "width: 25%")) "Connections")
              (th (@ (style "width: 25%")) "Graphs")
              (th (@ (style "width: 25%")) "Types")
              (th (@ (style "width: 25%")) "Predicates")))
         (tbody
          (tr (td (select (@ (name "connections")
                             (id "connections")
                             (class "connection-selector multiple-selector")
                             (size "2")
                             (multiple "multiple"))))
              (td (select (@ (name "graphs")
                             (id "graphs")
                             (class "graphs-selector multiple-selector")
                             (multiple "multiple")
                             (disabled "disabled")))
              (td (select (@ (name "types")
                             (id "types")
                             (class "types-selector multiple-selector")
                             (multiple "multiple")
                             (disabled "disabled"))))
              (td (select (@ (name "predicates")
                             (id "predicates")
                             (class "predicates-selector multiple-selector")
                             (multiple "multiple")
                             (disabled "disabled")))))))))
              
       (script "
$(document).ready(function(){
  $.get('/connections.json', function(data){
    var connections = JSON.parse(data);
    connections.map(function (connection){
      $('#connections').append('" (option (@ (value "'+ connection +'"))
                                          "'+ connection +'") "');
    });
  });

  $('#connections').on('change', function(){
    $('#graphs').prop('disabled', false);
    $('#graphs').find('option').remove()
    $('#types').find('option').remove()
    $('#predicates').find('option').remove()

    var connection = $('#connections option:selected' ).val();
    post_data = { connection: connection };
    $.post('/graphs.json', JSON.stringify(post_data), function(data){
      var graphs = JSON.parse(data);
      graphs.map(function (graph){
        $('#graphs').append('" (option (@ (value "'+ graph +'"))
                                            "'+ graph +'") "');
      });
    });
  });

  $('#graphs').on('change', function(){
    $('#types').prop('disabled', false);
    $('#types').find('option').remove()
    $('#predicates').find('option').remove()
    var connection = $('#connections option:selected' ).val();
    var graph      = $('#graphs option:selected' ).val();
    post_data = { connection: connection, graph: graph };
    $.post('/types.json', JSON.stringify(post_data), function(data){
      var types = JSON.parse(data);
      types.map(function (type){
        $('#types').append('" (option (@ (value "'+ type +'"))
                                      "'+ type +'") "');
      });
    });
  });

  $('#types').on('change', function(){
    $('#predicates').prop('disabled', false);
    $('#predicates').find('option').remove()
    var connection = $('#connections option:selected' ).val();
    var graph      = $('#graphs option:selected' ).val();
    var type       = $('#types option:selected' ).val();
    post_data = { connection: connection, graph: graph, type: type };
    $.post('/predicates.json', JSON.stringify(post_data), function(data){
      var predicates = JSON.parse(data);
      predicates.map(function (predicate){
        $('#predicates').append('" (option (@ (value "'+ predicate +'"))
                                      "'+ predicate +'") "');
      });
    });
  });

});"))
     #:dependencies '(jquery d3))))
