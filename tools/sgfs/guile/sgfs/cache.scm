;;; Copyright © 2020  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (sgfs cache)
  #:use-module (ice-9 threads)

  #:export (add-project-to-cache
            add-query-to-cache

            projects
            queries

            set-projects-lock!
            set-queries-lock!))

(define %projects-lock '())
(define %queries-lock '())

(define %projects '())
(define %queries  '())

(define (add-project-to-cache project)
  (lock-mutex %projects-lock)
  (set! %projects (cons project %projects))
  (unlock-mutex %projects-lock))

(define (add-query-to-cache query)
  (lock-mutex %queries-lock)
  (set! %queries (cons query %queries))
  (unlock-mutex %queries-lock))

(define (projects) %projects)
(define (queries)  %queries)

(define (set-projects-lock! lock) (set! %projects-lock lock))
(define (set-queries-lock! lock)  (set! %queries-lock lock))
