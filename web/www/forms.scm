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
            input-with-value))

(define (ul-without-bullets . body)
  `(ul (@ (class "ul-no-bullets"))
       ,body))

(define* (radio-button id name label #:key (checked? #f))
  `(span (input (@ (type "radio")
                   ,@(if checked? `((checked "")) '())
                   (id ,id)
                   (name ,name)
                   (value ,id))
                (label (@ (for ,id)) ,label))))

(define* (checkbox id text #:key (checked? #f) (required? #f))
  `(span (input (@ (type  "checkbox")
                   ,@(if checked? `((checked "")) '())
                   ,@(if required? `((required "")) '())
                   (id    ,id)
                   (name  ,id)
                   (class "form-checkbox")))
         (label (@ (for ,id)) ,text)))

(define* (input-with-value name type value #:key (required? #f))
  `(input (@ (id    ,name)
             (name  ,name)
             ,@(if required? `((required "")) '())
             (type  ,type)
             (value ,(if value value "")))))
