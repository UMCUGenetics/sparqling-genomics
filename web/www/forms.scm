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

(define-module (www forms)
  #:export (checkbox
            radio-button
            ul-without-bullets
            input-with-value
            required-marking))

(define required-marking
  '(style "border: solid 2pt #990000; background: #ffcccc"))

(define* (ul-without-bullets mark-required? . body)
  "MARK-REQUIRED? must be either #t or #f, and body must be one or
more SXML expressions."
  `(ul (@ (class "ul-no-bullets")
          ,@(if mark-required? `(,required-marking) '()))
       ,body))

(define* (radio-button id name label #:key (checked? #f))
  `(span (@ (class "form-radio-button"))
         (input (@ (type "radio")
                   ,@(if checked? `((checked "")) '())
                   (id ,id)
                   (name ,name)
                   (value ,id)))
         (label (@ (for ,id)) ,label)))

(define* (checkbox id text #:key (checked? #f)
                                 (required? #f)
                                 (show-missing? #f)
                                 (class #f)
                                 (onchange #f))
    `(span (@ (class "form-checkbox"))
           (input (@ (type  "checkbox")
                     ,@(if checked? `((checked "")) '())
                     ,@(if required? `((required "")) '())
                     ,@(if class `((class ,class)) '())
                     ,@(if onchange `((onchange ,onchange)) '())
                     (id    ,id)
                     (name  ,id)))
           (label (@ (for ,id)
                     ,@(if (and (not checked?) show-missing?)
                           `(,required-marking)
                           '()))
                  ,text)))

(define* (input-with-value name type value #:key (required? #f)
                                                 (show-missing? #f)
                                                 (placeholder #f))
  (let ((misses-value (or (not (string? value))
                          (string= value ""))))
    `(input (@ (id    ,name)
               (name  ,name)
               ,@(if required? `((required "")) '())
               ,@(if (and show-missing? misses-value)
                     `(,required-marking)
                     '())
               ,@(if placeholder `((placeholder ,placeholder)) '())
               (type  ,type)
               (value ,(if value value ""))))))
