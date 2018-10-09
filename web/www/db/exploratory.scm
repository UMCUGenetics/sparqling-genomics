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

(define-module (www db exploratory)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (www db connections)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (web response)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)

  #:export (all-graphs
            all-predicates-in-graph
            all-types-in-graph))

(define* (all-graphs #:key (connection #f))
  (if connection
      (apply append
             (query-results->list
              (sparql-query
               "SELECT DISTINCT ?graph WHERE { GRAPH ?graph { ?s ?p ?o } }"
               #:uri           (connection-uri connection)
               #:store-backend (connection-backend connection)
               #:digest-auth   (if (and (connection-username connection)
                                        (connection-password connection))
                                   (string-append
                                    (connection-username connection) ":"
                                    (connection-password connection))
                                   #f))
              #t))
      (apply append (par-map all-graphs (all-connections username)))))

(define (all-predicates-in-graph graph connection type)
  (if connection
      (apply append
             (query-results->list
              (sparql-query
               (string-append
                "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
                "SELECT DISTINCT ?predicate WHERE { GRAPH <"
                graph "> { ?s rdf:type <" type "> . ?s ?predicate ?o } }")
               #:uri           (connection-uri connection)
               #:store-backend (connection-backend connection)
               #:digest-auth   (if (and (connection-username connection)
                                        (connection-password connection))
                                   (string-append
                                    (connection-username connection) ":"
                                    (connection-password connection))
                                   #f)) #t))
      #f))

(define (all-types-in-graph graph connection)
  (if connection
      (apply append
             (query-results->list
              (sparql-query
               (string-append
                "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
                "SELECT DISTINCT ?type WHERE { GRAPH <" graph "> "
                "{ ?s rdf:type ?type } }")
               #:uri           (connection-uri connection)
               #:store-backend (connection-backend connection)
               #:digest-auth   (if (and (connection-username connection)
                                        (connection-password connection))
                                   (string-append
                                    (connection-username connection) ":"
                                    (connection-password connection))
                                   #f)) #t))
      #f))
