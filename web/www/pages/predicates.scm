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

(define-module (www pages predicates)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www db exploratory)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:export (page-predicates))

;; ----------------------------------------------------------------------------
;; PAGE-PREDICATES
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-predicates request-path username
                      #:key (post-data #f)
                      (type 'json))
  (if post-data
      (let ((data       (json-string->scm post-data)))
        (let* ((conn-name  (hash-ref data "connection"))
               (connection (connection-by-name conn-name username))
               (graph      (hash-ref data "graph"))
               (rdf-type   (hash-ref data "type")))
          (cond
           [(eq? type 'json)
            (scm->json-string (all-predicates-in-graph graph connection rdf-type))])))
      "[]"))
