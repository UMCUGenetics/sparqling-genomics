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

(define-module (www pages query-history)
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

  #:export (page-query-history))

(define (strip-prefix-lines query)
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (line)
                  (unless (or (string-prefix? "prefix" line)
                              (string-prefix? "PREFIX" line)
                              (string= line ""))
                    (format port "~a~%" line)))
                (string-split query #\newline)))))

(define* (page-query-history request-path #:key (post-data ""))
  (let* ((project     (active-project))
         (queries     (queries-by-project (project-name project))))
    `(table (@ (id "item-table"))
            (tr (th "Query")
                (th "Connection")
                (th (@ (colspan "2")) "Actions"))
            ,(map (lambda (query)
                    `(tr (td (pre ,(strip-prefix-lines (query-content query))))
                         (td ,(query-endpoint query))
                         (td (@ (class "button-column left-button-column"))
                             (form (@ (action "/query") (method "post"))
                                   (button (@ (type "submit")
                                              (class "action-btn remove-btn")
                                              (name "remove")
                                              (value ,(query-id query)))
                                           "✖")))
                         (td (@ (class "button-column right-button-column"))
                             (form (@ (action "/query") (method "post"))
                                   (input (@ (type "hidden")
                                             (name "endpoint")
                                             (value ,(query-endpoint query))))
                                   (button (@ (type "submit")
                                              (class "action-btn insert-btn")
                                              (name "query")
                                              (value ,(query-content query)))
                                           "⤴")))))
                  queries)
            '())))
