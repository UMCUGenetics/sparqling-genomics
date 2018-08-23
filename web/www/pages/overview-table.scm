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

(define-module (www pages overview-table)
  #:use-module (www pages)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db overview)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)

  #:export (page-overview-table))

(define* (page-overview-table request-path #:key (post-data ""))
  (let* ((info (par-map (lambda (func) (func))
                        (list number-of-samples
                              number-of-variant-calls
                              number-of-copynumber-calls))))
    `(table (@ (id "item-table"))
      (tr (th "Property")
          (th "Value"))
      (tr (td "Number of samples")
          (td ,(list-ref info 0)))
      (tr (td "Number of variant calls (may contain duplicates)")
          (td ,(list-ref info 1)))
      (tr (td "Number of copy number calls (may contain duplicates)")
          (td ,(list-ref info 2))))))
