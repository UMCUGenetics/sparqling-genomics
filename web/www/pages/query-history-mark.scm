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

(define-module (www pages query-history-mark)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db queries)

  #:export (page-query-history-mark))

(define* (page-query-history-mark request-path #:key (post-data ""))
  (if (string= post-data "")
      '(p "Please send a POST request with a SPARQL query.")
      (let* ((parsed-data (json-string->scm post-data))
             (state       (hash-ref parsed-data "state"))
             (query-id    (hash-ref parsed-data "query-id"))
             (query (query-by-id query-id)))
        (set-query-marked! query state)
        (persist-queries)
        (string-append "[{\"state\": " (if state "true" "false") "}]"))))
