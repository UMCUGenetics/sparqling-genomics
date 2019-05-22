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
  #:use-module (ice-9 format)
  #:use-module (logger)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (www config)
  #:use-module (www util)

  #:export (all-datasets
            filtered-datasets
            filtered-datasets-query
            all-collections
            all-collections-query

            all-assemblies
            all-assemblies-query
            assembly-title

            graphs-for-dataset

            dataset-id
            dataset-title
            dataset-description
            dataset-publisher

            collection-title
            collection-description
            collection-publisher))

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
    ?id        dc:description    ?description .
    ?id        dcterms:publisher ?pub .
    ?pub       rdfs:label        ?publisher .
  }
}"))
             (entries (query-results->alist (system-sparql-query query)))]
        entries))
    (lambda (key . args)
      (log-error "all-datasets"
                 "Unknown exception thrown in ~a: ~a: ~a~%" key args)
      '())))

(define (filtered-datasets-query filters)
  (let [(collections (hash-ref filters "collections"))
        (assemblies  (hash-ref filters "assemblies"))]
    (string-append
     default-prefixes
     "
SELECT DISTINCT ?id ?title ?description ?publisher
WHERE {
  ?id         rdf:type                  dctype:Dataset ;
              dcterms:title             ?title ;
              dcterms:isPartOf          ?collection .

  OPTIONAL {
    ?id        dc:description    ?description .
    ?id        dcterms:publisher ?pub .
    ?pub       rdfs:label        ?publisher .
  }
  OPTIONAL {
    ?collection rdf:type                  dctype:Collection ;
                dcterms:title             ?collectionName .
  }
  OPTIONAL {
    ?id         sg:mappedToGenomeAssembly ?assembly .
    ?assembly   rdf:type                  ncbi:HaploidAltAssembly ;
                dcterms:title             ?assemblyName .
  }
"
     (if (null? collections)
         ""
         (format #f "  FILTER ((?collectionName = ~s)~{ OR (?collectionName = ~s)~})~%"
                 (car collections)
                 (cdr collections)))
     (if (null? assemblies)
         ""
         (format #f "  FILTER ((?assemblyName = ~s)~{ OR (?assemblyName = ~s)~})~%"
                 (car assemblies)
                 (cdr assemblies)))
     "
}")))

(define (filtered-datasets filters)
  (catch #t
    (lambda _
      (query-results->alist (system-sparql-query
                             (filtered-datasets-query filters))))
    (lambda (key . args)
      (log-error "all-datasets"
                 "Unknown exception thrown in ~a: ~a: ~a~%" key args)
      '())))

;; ----------------------------------------------------------------------------
;; RECORDS-LIKE INTERFACE FOR DATASETS
;; ----------------------------------------------------------------------------

(define-syntax-rule
  (dataset-id dataset)
  (assoc-ref dataset "id"))

(define-syntax-rule
  (dataset-title dataset)
  (assoc-ref dataset "title"))

(define-syntax-rule
  (dataset-description dataset)
  (assoc-ref dataset "description"))

(define-syntax-rule
  (dataset-publisher dataset)
  (assoc-ref dataset "publisher"))


;; ----------------------------------------------------------------------------
;; ALL-COLLECTIONS
;; ----------------------------------------------------------------------------

(define all-collections-query
  (string-append
   default-prefixes
   "
SELECT DISTINCT ?title ?description ?publisher
WHERE {
  ?subject rdf:type       dctype:Collection ;
           dcterms:title  ?title .
  OPTIONAL {
    ?subject   dc:description    ?description ;
               dcterms:publisher ?pub .
    ?pub       rdfs:label        ?publisher .
  }
}"))

(define (all-collections)
  (catch #t
    (lambda _
      (query-results->alist (system-sparql-query all-collections-query)))
    (lambda (key . args)
      '())))

;; ----------------------------------------------------------------------------
;; RECORDS-LIKE INTERFACE FOR COLLECTIONS
;; ----------------------------------------------------------------------------

(define-syntax-rule
  (collection-title collection)
  (assoc-ref collection "title"))

(define-syntax-rule
  (collection-description collection)
  (assoc-ref collection "description"))

(define-syntax-rule
  (collection-publisher collection)
  (assoc-ref collection "publisher"))

;; ----------------------------------------------------------------------------
;; ALL-ASSEMBLIES
;; ----------------------------------------------------------------------------

(define all-assemblies-query
  "PREFIX dcterms:     <http://purl.org/dc/terms/>
PREFIX rdf:         <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ncbi:        <http://rdf.biosemantics.org/ontologies/ncbiassembly#>

SELECT DISTINCT ?title
WHERE
{
  ?assembly rdf:type      ncbi:HaploidAltAssembly ;
            dcterms:title ?title .
}")

(define (all-assemblies)
  (catch #t
    (lambda _
      (query-results->alist (system-sparql-query all-assemblies-query)))
    (lambda (key . args)
      '())))

;; ----------------------------------------------------------------------------
;; RECORDS-LIKE INTERFACE FOR ASSEMBLIES
;; ----------------------------------------------------------------------------

(define-syntax-rule
  (assembly-title assembly)
  (assoc-ref assembly "title"))

;; ----------------------------------------------------------------------------
;; GRAPHS
;; ----------------------------------------------------------------------------

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
      (log-error "graphs-for-dataset"
                 "Unknown exception thrown in ~a: ~a: ~a~%" key args)
      '())))
