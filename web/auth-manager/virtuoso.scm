;;; Copyright © 2020  Roel Janssen <roel@gnu.org>
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

(define-module (auth-manager virtuoso)
  #:use-module (auth-manager config)

  #:export (stage-file start-bulk-load))

(define (stage-file filename graph-uri)
  (zero?
   (system (format #f
            "printf \"ld_dir ('~a', '~a', '~a');\n\" | ~a ~a -U ~a -P ~a~%"
            (dirname filename) (basename filename) graph-uri
            (isql-bin) (isql-port)
            (rdf-store-username) (rdf-store-password)))))

(define (start-bulk-load)
  (zero?
   (system (format #f "printf \"rdf_loader_run ();\n\" | ~a ~a -U ~a -P ~a~%"
                   (isql-bin) (isql-port)
                   (rdf-store-username) (rdf-store-password)))))

(define (load-file filename graph-uri)
  (stage-file filename graph-uri)
  (start-bulk-load))
