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
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)

  #:export (stage-file start-bulk-load virtuoso-isql-query))

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

(define (virtuoso-isql-query query)
  "Executes QUERY via ISQL and returns a both an ERROR-PORT and a PORT to read CSV output from."
  (let* ((tmp        (getenv "TMPDIR"))
         (error-port (mkstemp! (string-append (if tmp tmp "/tmp") "/sg-XXXXXX")))
         (port       (open-input-pipe
                      (format #f "~a ~a -U ~a -P ~a verbose=off csv_rfc4180=on csv_rfc4180_field_separator=, exec='~:a' 2> ~a"
                              (isql-bin) (isql-port) (rdf-store-username)
                              (rdf-store-password)
                              (string-append "SPARQL " query)
                              (port-filename error-port)))))
    (setvbuf port 'block 4096)
    (values error-port port)))

(define (load-file filename graph-uri)
  (stage-file filename graph-uri)
  (start-bulk-load))
