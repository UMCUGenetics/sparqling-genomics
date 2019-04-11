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

(define-module (www pages project-dependent-graphs)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:use-module (sxml simple)

  #:export (page-project-dependent-graphs))

(define* (page-project-dependent-graphs request-path username #:key (post-data ""))
  (let* [(hash        (basename request-path))
         (project     (project-by-hash hash))
         (project-uri (project-id project))
         (graphs      (project-inferred-graphs project-uri))]
    (if (null? graphs)
        '(p "Could not derive associated graphs from queries.")
        `((table (@ (class "item-table"))
                 (tr (th "Graph"))
                 ,(map (lambda (item)
                         `(tr (td ,item)))
                       graphs))))))
