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

(define-module (www db portal)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (sparql driver)
  #:use-module (sparql util)

  #:export (all-datasets
            all-collections
            graphs-for-dataset))

;; ----------------------------------------------------------------------------
;; ALL-DATASETS
;; ----------------------------------------------------------------------------

(define (all-datasets)
  (catch #t
    (lambda _
      (let* [(query (string-append
                     default-prefixes
                     "SELECT DISTINCT ?id ?title ?description ?publisher
WHERE {
  ?id      rdf:type       dctype:Dataset ;
           dcterms:title  ?title .
  OPTIONAL {
    ?id        dc:description    ?description ;
               dcterms:publisher ?pub .
    ?pub       rdfs:label        ?publisher .
  }
}"))
             (entries (query-results->alist (system-sparql-query query)))]
        entries))
    (lambda (key . args)
      (format #t "Unknown exception thrown in ~a: ~a: ~a~%"
              "all-datasets" key args)
      '())))

;; ----------------------------------------------------------------------------
;; ALL-COLLECTIONS
;; ----------------------------------------------------------------------------

(define (all-collections)
  (catch #t
    (lambda _
      (let* [(query (string-append
                     default-prefixes
                     "SELECT DISTINCT ?title ?description ?publisher
WHERE {
  ?subject rdf:type       dctype:Collection ;
           dcterms:title  ?title .
  OPTIONAL {
    ?subject   dc:description    ?description ;
               dcterms:publisher ?pub .
    ?pub       rdfs:label        ?publisher .
  }
}"))
             (entries (query-results->alist (system-sparql-query query)))]
        entries))
    (lambda (key . args)
      '())))

(define (graphs-for-dataset id)
  (catch #t
    (lambda _
      (let* [(query (string-append
                     default-prefixes
                     "SELECT DISTINCT ?graph "
                     "WHERE { ?graph sg:containsDataFor <" id "> . }"))
             (entries (query-results->alist (system-sparql-query query)))]
        entries))
    (lambda (key . args)
      (format #t "Unknown exception thrown in ~a: ~a: ~a~%"
              "all-datasets" key args)
      (format #t "Argument was: ~s~%" id)
      '())))
