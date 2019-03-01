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

(define* (page-datasets request-path username #:key (post-data ""))
  (page-root-template "Data collections" request-path
   `((h2 "Data collections")
     ,(map (lambda (collection)
             `(div (@ (class "data-collection"))
                   (h2 ,(assoc-ref collection "name") " ("
                       ,(assoc-ref collection "publisher") ")")
                   (p ,(assoc-ref collection "description"))))
           (all-datasets (default-connection username))))
   #:dependencies '()))

