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

(define-module (www pages portal)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db connections)
  #:use-module (www db overview)
  #:use-module (www db projects)
  #:use-module (www db portal)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:export (page-portal))

(define (%query-endpoint username)
  (let ((connections (all-connections username)))
    (if (null? connections)
        #f
        (connection-name (car connections)))))

(define (make-query-button text query username)
  (if (%query-endpoint username)
      `(div (@ (class "show-me-in-h2 small-action-btn"))
            (form (@ (action "/query") (method "post"))
                  (input (@ (type "hidden")
                            (name "endpoint")
                            (value ,(%query-endpoint username))))
                  (button (@ (type "submit")
                             (id "query-button")
                             (class "small-action-btn question-btn")
                             (name "query")
                             (value ,query))
                          ,text)))
      text))

(define (show-datasets-table datasets)
  `(div (@ (id "datasets"))
    ,(map
      (lambda (dataset)
        (let [(graphs (graphs-for-dataset (dataset-id dataset)))]
          `(div (@ (class "dataset"))
                (h2 ,(dataset-title dataset)
                    ,(if (dataset-publisher dataset)
                         `(span (@ (class "side-info")) " by "
                                ,(dataset-publisher dataset))))
                (p ,(assoc-ref dataset "description"))
                ,(if (not (null? graphs))
                     `(p (strong "Graph" ,(if (> (length graphs) 1)
                                              "s" "") ":")
                         " "
                         ,(map (lambda (graph)
                                 `(code ,(assoc-ref graph "graph")))
                               graphs))
                     '()))))
      datasets)))

(define* (page-portal request-path username #:key (post-data #f))
  (if post-data
      (show-datasets-table
       (filtered-datasets (json-string->scm post-data)))
      (page-root-template username "Portal" request-path
       `((h2 "Portal")
         (div (@ (id "two-column"))
          (div (@ (id "two-column-left-side"))
           (h3 "Filter"
               ,(if username
                    (make-query-button "SHOW ME" all-collections-query username)
                    '()))
           (p "")
           (div (@ (id "sidebar"))
            (form (@ (id "sidebar-form"))
             (table (@ (class "sidebar"))
              (tr (th "Collections"))
              ,(map (lambda (collection)
                      (let [(title (collection-title collection))]
                        `(tr (td (label (input (@ (type "checkbox")
                                                  (name "collection")
                                                  (value ,title)))
                                        ,title)))))
                    (all-collections)))

             (table (@ (class "sidebar"))
              (tr (th "Reference assembly"))
              ,(map (lambda (assembly)
                      (let [(title (assembly-title assembly))]
                        `(tr (td (label
                                  (input (@ (type "checkbox")
                                            (name "assembly")
                                            (value ,title)))
                                  ,title)))))
                    (all-assemblies))))))

          (div (@ (id "two-column-right-side"))
               (h3 "Datasets")
               (p "")
               ,(show-datasets-table (all-datasets))))
         (script "
$(document).ready(function(){
  $('#sidebar-form').change(filter_items);
});"))
       #:dependencies '(jquery portal))))
