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

(define-module (www pages datasets)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db connections)
  #:use-module (www db overview)
  #:use-module (www db projects)
  #:use-module (www db datasets)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-datasets))

(define (%query-endpoint username)
  (let ((connections (all-connections username)))
    (if (null? connections)
        #f
        (connection-name (car connections)))))

(define %default-filter-query
  (string-append
   default-prefixes
   "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"))

(define (make-query-button text query username)
  (if (%query-endpoint username)
      `(div (@ (class "show-me-in-h2 small-action-btn"))
            (form (@ (action "/query") (method "post"))
                  (input (@ (type "hidden")
                            (name "endpoint")
                            (value ,(%query-endpoint username))))
                  (button (@ (type "submit")
                             (class "small-action-btn question-btn")
                             (name "query")
                             (value ,query))
                          ,text)))
      text))

(define* (page-datasets request-path username #:key (post-data ""))
  (page-root-template username "Data collections" request-path
   `((h2 "Portal")
     (div (@ (id "two-column"))
          (div (@ (id "two-column-left-side"))
               (h3 "Filter"
                   ,(if username
                        (make-query-button "SHOW ME"
                                           %default-filter-query
                                           username)
                        '()))
               (p "")
               (div (@ (id "sidebar"))
                    (form
                     (table (@ (class "sidebar"))
                            (tr (th "Collections"))
                            ,(map (lambda (collection)
                                    `(tr (td (label (input (@ (type "checkbox")))
                                                    ,(assoc-ref collection "title")))))
                                  (all-collections))))))

          (div (@ (id "two-column-right-side"))
               (h3 "Datasets")
               (p "")
               ,(map
                 (lambda (dataset)
                   (let [(graphs (graphs-for-dataset
                                  (assoc-ref dataset "id")))]
                     `(div (@ (class "dataset"))
                           (h2 ,(assoc-ref dataset "title")
                               ,(if (assoc-ref dataset "publisher")
                                    `(span (@ (class "side-info")) " by "
                                           ,(assoc-ref dataset "publisher"))))
                           (p ,(assoc-ref dataset "description"))
                           ,(if (not (null? graphs))
                                `(p (strong "Graph" ,(if (> (length graphs) 1) "s" "") ":")
                                    " "
                                    ,(map (lambda (graph)
                                            `(code ,(assoc-ref graph "graph")))
                                          graphs))
                                '()))))
                 (all-datasets)))))
   #:dependencies '()))
