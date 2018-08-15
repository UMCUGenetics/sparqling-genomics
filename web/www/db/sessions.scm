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

(define-module (www db sessions)
  #:use-module (www util)
  #:use-module (www base64)
  #:use-module (www config)
  #:use-module (sparql lang)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)

  #:export (session-add
            session-edit
            session-remove
            all-sessions
            session-by-username
            session-by-token
            load-sessions
            persist-sessions

            alist->session
            session->alist

            make-session
            session-username
            session-token
            session?

            is-valid-session-token?
            set-session-username!
            set-session-token!))

(define %db-sessions '())

;; SESSION RECORD TYPE
;; ----------------------------------------------------------------------------
(define-record-type <session>
  (make-session username token)
  session?
  (username      session-username   set-session-username!)
  (token         session-token      set-session-token!))


;; ALIST->SESSION AND SESSION->ALIST
;; ----------------------------------------------------------------------------
(define (alist->session input)
  "Turns the association list INPUT into a session record."
  (catch #t
    (lambda _
      (let ((obj (make-session (assoc-ref input 'username)
                               (assoc-ref input 'token))))
        ;; The token will be automatically generated when none is provided.
        ;; For this we generate 64 random numbers between 0 and 256, and
        ;; we base64 it, so that it's web-compatible.
        (when (and (string? (session-token obj))
                   (string= (session-token obj) ""))
          (set-session-token! obj (base64-encode
                                   (u8-list->bytevector
                                    (map (lambda _ (random 256)) (iota 64))))))
        obj))
    (lambda (key . args)
      #f)))

(define (session->alist record)
  `((username . ,(session-username record))
    (token    . ,(session-token    record))))

;; SESSIONS PERSISTENCE
;; ----------------------------------------------------------------------------
(define (load-sessions)
  (catch #t
    (lambda _
      (let ((filename (string-append (www-cache-root) "/sessions.scm")))
        (when (file-exists? filename)
          (call-with-input-file filename
            (lambda (port)
              (set! %db-sessions
                    (map alist->session (read port))))))))
    (lambda (key . args)
      #f)))

(define (persist-sessions)
  (let ((filename (string-append (www-cache-root) "/sessions.scm")))
    (call-with-output-file filename
      (lambda (port)
        ;; Before writing to the file under 'port', chmod it so that
        ;; only the user this process runs as can read its contents.
        (chmod port #o600)
        (format port ";; This file was generated by sparqling-genomics.~%")
        (format port ";; Please do not edit this file manually.~%")
        (write (map session->alist %db-sessions) port)))))

;; SESSION-ADD
;; ----------------------------------------------------------------------------
(define (session-add record)
  "Adds a reference to the internal graph for the session RECORD."
  (let ((username (session-username record))
        (token    (session-token    record)))
    (cond
     ((string= username "")
      (values #f (format #f "The username cannot empty.")))
     ((string= token "")
      (values #f (format #f "The session token cannot empty.")))
     (#t (begin
           (set! %db-sessions (cons record %db-sessions))
           (persist-sessions)
           (values #t ""))))))

;; SESSION-REMOVE
;; ----------------------------------------------------------------------------
(define (session-remove session)
  "Removes the reference in the internal graph for SESSION."
  (let ((token (if (string? session) session (session-token session))))
    (set! %db-sessions
          (filter (lambda (record)
                    (not (string= (session-token record) token)))
                  %db-sessions))
    (persist-sessions)
    (values #t (format #f "Removed “~a”." token))))


;; ALL-SESSIONS
;; ----------------------------------------------------------------------------

(define* (all-sessions #:key (filter #f))
  "Returns a list of session records, applying FILTER to the records."
  (if filter
      (map filter %db-sessions)
      %db-sessions))

(define (session-by-username username)
  (let ((item (filter (lambda (session)
                        (string= (session-username session) username))
                      %db-sessions)))
    (if (null? item)
        #f
        (car item))))

(define (session-by-token token)
  (let ((item (filter (lambda (session)
                        (string= (session-token session) token))
                      %db-sessions)))
    (if (null? item)
        #f
        (car item))))

(define (is-valid-session-token? token)
  (if (session-by-token token) #t #f))