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

(define-module (www pages edit-project)
  #:use-module (www pages)
  #:use-module (www pages projects)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www db projects)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (page-edit-project))

;; ----------------------------------------------------------------------------
;; PAGE-EDIT-PROJECT
;; ----------------------------------------------------------------------------
;;
;; This function describes the SXML equivalent of the entire web page.
;;

(define* (page-edit-project request-path #:key (post-data ""))


  (define* (deduplicate-alist alist #:optional (name #f) (samples '()))
    (if (null? alist)
        `((name    . ,name)
          (samples . ,samples))
        (let* ((item  (car alist))
               (key   (car item))
               (value (cdr item)))
          (cond
           [(eq? key 'name)
            (deduplicate-alist (cdr alist) value samples)]
           [(eq? key 'sample)
            (deduplicate-alist (cdr alist) name (cons value samples))]))))

  (let* ((name (last (string-split request-path #\/)))
         (message
          (if (not (string= post-data ""))
              (receive (success? message)
                  (let ((alist (deduplicate-alist
                                (post-data->alist
                                 (uri-decode post-data)))))
                    (match alist
                      (((name . a) (samples . b))
                       (project-edit (alist->project alist)))
                      (else
                       (values #f "Invalid form data."))))
                (if success?
                    #f ; No need to display a message.
                    `(div (@ (class "message-box failure")) (p ,message))))
              #f))
         (project (project-by-name name))
         (title (string-append "Edit “" name "”")))
    (page-root-template title request-path
     `((h2 ,title)
       ;; When an action occurred (like “the project was modified”), we
       ;; display the success or error message accordingly.
       ,(if message message '())

       ;; Display the main table.
       (form (@ (autocomplete "off")
                (action ,(string-append "/edit-project/" name))
                (method "post"))
             (input (@ (type "hidden")
                       (name "name")
                       (value ,(project-name project))))
             (table (tr
                     (td "Name")
                     (td (@ (style "width: 90%"))
                         (input (@ (type "text")
                                   (id "add-name-field")
                                   (name "name-disabled")
                                   (value ,(project-name project))
                                   (placeholder "Name")
                                   (disabled "disabled")))))
                    (tr
                     (td "Samples")
                     (td (p "Select the samples to include in this project.")
                         (div (@ (id "samples")))))
                    (tr
                     (td "")
                     (td (@ (class "item-table-right"))
                            (input (@ (id "edit-button")
                                      (type "submit")
                                      (value "Save modifications")))))))
       (script "
$(document).ready(function(){
  $.get('/samples.json',function(samples) {
      var samples = JSON.parse(samples);
      if (samples.length == 0) {
        $('#insert-samples').text('Could not find any samples in the SPARQL endpoints.');
      }
      else {
        samples.forEach(function(sample){
          $('#samples').append('"
               (span (@ (class "sample-field"))
                 (input (@ (type "checkbox")
                           (id "sample-'+ sample +'")
                           (name "sample")
                           (value "'+ sample +'")) "'+ sample +'")) "');
          });

          $.get('/project-samples/" ,name ".json',function(samples) {
              samples = JSON.parse(samples);
              samples.forEach(function(sample) {
                $('#sample-'+ sample).prop('checked', true);
              });
          });
      }
  });
});
"))
     #:dependencies '(jquery))))
