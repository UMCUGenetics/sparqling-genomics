;;; Copyright © 2017, 2018  Roel Janssen <roel@gnu.org>
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

(define-module (www pages getting-started)
  #:use-module (www pages)
  #:use-module (www config)
  #:export (page-getting-started))

(define (page-getting-started request-path)
  (page-root-template "Getting started" request-path
   `((h2 "Getting started")
     (h3 "Configure a connection")
     (p "Before a query can be performed, a way of connecting to the database"
        " must be configured.  This can be done on the "
        (a (@ (href "/connections")) "connections") " page.")

     (h3 "Querying a database")
     (p "You can query the database using the "
        (a (@ (href "/query")) "query interface") "."))))
