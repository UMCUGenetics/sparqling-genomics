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

(define-module (www pages collect)
  #:use-module (www pages)
  #:export (page-collect))

(define* (page-collect request-path username hash #:key (post-data ""))
  (page-root-template username "Collect" request-path
   `((h2 "Collect")

     (h3 "Import RDF")
     (p "The " (i "import") " feature allows uploading a file to an RDF"
        " store using HTTP.")

     (div (@ (class "large-action-btn"))
          (a (@ (href ,(string-append "/import/" hash)))
             "Import data")))
      #:dependencies '(jquery)))

