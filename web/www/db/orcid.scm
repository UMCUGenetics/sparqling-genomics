;;; Copyright © 2020  Roel Janssen <roel@gnu.org>
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

(define-module (www db orcid)
  #:use-module (ice-9 receive)
  #:use-module (logger)
  #:use-module (rnrs io ports)
  #:use-module (sparql util)
  #:use-module (web response)
  #:use-module (www config)
  #:use-module (www util)

  #:export (orcid-record-by-user
            persist-orcid-record))

(define (persist-orcid-record record)
  (let* ((id        (assoc-ref record 'id))
         (name      (assoc-ref record 'name))
         (query     (string-append
                     internal-prefixes
                     "INSERT INTO <" system-state-graph "> {"
                     "agent:" id
                     " rdfs:label \"" name "\"^^xsd:string ;"
                     " sg:orcidUri <https://orcid.org/" id "> . "
                     "}")))
    (receive (header body)
        (system-sparql-query query)
      (if (= (response-code header) 200)
          #t
          (begin
            (display (get-string-all body))
            #f)))))

(define (orcid-record-by-user username)
  (let ((query (string-append
                internal-prefixes
                "SELECT ?id ?name ?uri FROM <" system-state-graph "> { "
                "agent:" username " rdfs:label ?name ; sg:orcidUri ?uri .")))
    (query-results->alist (system-sparql-query query))))
