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
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-exploratory))

(define* (page-exploratory request-path username hash #:key (post-data ""))
  (page-root-template username "Exploratory" request-path
   `((h2 "Exploratory")
     (p "The exploratory provides an alternative interface to explore the "
        "structure of data available at each connection.  It is optimized "
        "for speed, allowing it to show outdated information.  By using "
        "the ↻ button, you can request the most recent data.  This may "
        "be a bit slower than showing the outdated information.")
     (form
      (table (@ (id "exploratory-table")
                (class "item-table"))
       (thead
        (tr (th (@ (style "width: 25%;")) "Connections")
            (th (@ (style "width: 25%;")) "Graphs")
            (th (@ (style "width: 25%;")) "Types")
            (th (@ (style "width: 25%;")) "Predicates")))
       (tbody
        (tr (td (div (@ (name "connections")
                        (id "connections")
                        (class "connection-selector multiple-selector")
                        (size "100"))))
            (td (div (@ (name "graphs")
                        (id "graphs")
                        (class "graphs-selector multiple-selector")
                        (size "100")
                        (disabled "disabled"))))
            (td (div (@ (name "types")
                        (id "types")
                        (class "types-selector multiple-selector")
                        (size "100")
                        (disabled "disabled"))))
            (td (div (@ (name "predicates")
                        (id "predicates")
                        (class "predicates-selector multiple-selector")
                        (size "100")
                        (disabled "disabled")))))
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
                (style "position: absolute; width: 1px; height: 1px; padding:0; left: -9999px;")))))
   #:dependencies '(jquery exploratory)))
