;;; Copyright © 2019 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (logger)
  #:use-module (www config)
  #:export (log-error
            log-warning
            log-debug))

(define (log-any type port function fmt . rst)
  (unless (null? port)
    (format port "[ ~a ] ~a: ~a: " type
            (strftime "%Y-%m-%d %H:%M:%S" (gmtime (current-time)))
            (if (string? function) function "unknown"))
    (format port fmt rst)
    (force-output port)))

(define (log-debug function fmt . rst)
  (log-any "DEBUG" (default-debug-port) function fmt rst))

(define (log-warning function fmt . rst)
  (log-any "WARNING" (default-warning-port) function fmt rst))

(define (log-error function fmt . rst)
  (log-any "ERROR" (default-error-port) function fmt rst))
