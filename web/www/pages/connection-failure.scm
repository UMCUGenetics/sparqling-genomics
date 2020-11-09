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

(define-module (www pages connection-failure)
  #:use-module (www pages)
  #:export (page-connection-failure))

(define* (page-connection-failure request-path #:key (post-data ""))
  (page-empty-template "Connection failure" request-path
   `((h2 "Connection failure")
     (p "The web interface cannot reach the SPARQL endpoint that was "
        "configured as the " (code "system-connection") "."))))
