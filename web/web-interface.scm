;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
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

(define-module (web-interface)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (json)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www pages)
  #:use-module (www pages error)
  #:use-module (www pages welcome)

  #:export (run-web-interface))



;; ----------------------------------------------------------------------------
;; HANDLERS
;; ----------------------------------------------------------------------------
;;
;; The way a request is handled varies upon the nature of the request.  It can
;; be as simple as serving a pre-existing file, or as complex as finding a
;; Scheme module to use for handling the request.
;;
;; In this section, the different handlers are implemented.
;;

(define (request-file-handler path)
  "This handler takes data from a file and sends that as a response."

  (define (response-content-type path)
    "This function returns the content type of a file based on its extension."
    (let ((extension (substring path (1+ (string-rindex path #\.)))))
      (cond [(string= extension "css")  '(text/css)]
            [(string= extension "js")   '(application/javascript)]
            [(string= extension "json") '(application/javascript)]
            [(string= extension "html") '(text/html)]
            [(string= extension "png")  '(image/png)]
            [(string= extension "svg")  '(image/svg+xml)]
            [(string= extension "ico")  '(image/x-icon)]
            [(string= extension "pdf")  '(application/pdf)]
            [(string= extension "ttf")  '(application/font-sfnt)]
            [(#t '(text/plain))])))

  (let ((full-path (string-append %www-root "/" path)))
    (if (not (file-exists? full-path))
        (values '((content-type . (text/html)))
                (with-output-to-string (lambda _ (sxml->xml (page-error-404 path)))))
        ;; Do not handle files larger than %maximum-file-size.
        ;; Please increase the file size if your server can handle it.
        (let ((file-stat (stat full-path)))
          (if (> (stat:size file-stat) %www-max-file-size)
              (values '((content-type . (text/html)))
                      (with-output-to-string 
                        (lambda _ (sxml->xml (page-error-filesize path)))))
              (values `((content-type . ,(response-content-type full-path)))
                      (with-input-from-file full-path
                        (lambda _
                          (get-bytevector-all (current-input-port))))))))))

(define (request-scheme-page-handler request request-body request-path)

  (define (module-path prefix elements)
    "Returns the module path so it can be loaded."
    (append prefix (map string->symbol elements)))

  (define (resolve-module-function request-path)
    "Return FUNCTION from MODULE."
    (let* ((module (resolve-module (module-path '(www pages)
                     (string-split request-path #\/)) #:ensure #f))
           (page-symbol (symbol-append 'page-
                         (string->symbol
                          (string-replace-occurrence request-path #\/ #\-)))))
      ;; Return #f unless the 'page-symbol' exists in 'module',
      ;; in which case we return that.
      (if module
          (catch #t
            (lambda _ (module-ref module page-symbol))
            (lambda (key . args) #f))
          #f)))

  ;; Return-type handlers.
  ;; --------------------------------------------------------------------------
  (cond
   ;; The “/” page is special, because we re-route it to “welcome”.
   [(< (string-length request-path) 2)
    (values '((content-type . (text/html)))
            (call-with-output-string
              (lambda (port)
                (set-port-encoding! port "utf8")
                (format port "<!DOCTYPE html>~%")
                (sxml->xml (page-welcome "/") port))))]

   ;; When the “file extension” of the request indicates JSON, treat the
   ;; returned format as ‘application/javascript’.
   [(and (> (string-length request-path) 5)
         (string= (string-take-right request-path 5) ".json"))
    (values '((content-type . (application/javascript)))
            (call-with-output-string
              (lambda (port)
                (set-port-encoding! port "utf8")
                (let* ((request-path (basename request-path ".json"))
                       (page-function (resolve-module-function request-path)))
                  (if page-function
                      (if (eq? (request-method request) 'POST)
                          (display (page-function request-path
                                    #:post-data (utf8->string request-body))
                                   port)
                          (display (page-function request-path) port))
                      (display "{}" port))))))]

   ;; All other requests can be handles as HTML responses.
   [#t
    (values '((content-type . (text/html)))
            (call-with-output-string
              (lambda (port)
                (set-port-encoding! port "utf8")
                (format port "<!DOCTYPE html>~%")
                (let* ((path (substring request-path 1))
                       (page-function (resolve-module-function path)))
                  (if page-function
                      (if (eq? (request-method request) 'POST)
                          (sxml->xml (page-function request-path
                                      #:post-data (utf8->string request-body))
                                     port)
                          (sxml->xml (page-function request-path) port))
                      (sxml->xml (page-ontology-or-error-404 request-path)
                                 port))))))]))


;; ----------------------------------------------------------------------------
;; ROUTING & HANDLERS
;; ----------------------------------------------------------------------------
;;
;; Requests can have different handlers.
;; * Static objects (images, stylesheet, javascript files) have their own
;;   handler.
;; * Package pages are generated dynamically, so they have their own handler.
;; * The 'regular' Scheme pages have their own handler that resolves the
;;   module dynamically.
;;
;; Feel free to add your own handler whenever that is necessary.
;;

(define (request-handler request request-body)
  (let ((request-path (uri-path (request-uri request))))
    (cond
     ((and (> (string-length request-path) 7)
           (string= (string-take request-path 8) "/static/"))
      (request-file-handler request-path))
     (else
      (request-scheme-page-handler request request-body request-path)))))

;; ----------------------------------------------------------------------------
;; RUNNER
;; ----------------------------------------------------------------------------
;;
;; This code runs the web server.
;;

(define (run-web-interface)
  (let ((pid (primitive-fork)))
    (if (eq? pid 0)
        (begin
          (format #t "SPARQLing-SVs web service is running at http://127.0.0.1:~a~%"
                  %www-listen-port)
          (with-output-to-file "web.log"
            (lambda _
              (run-server request-handler 'http
                          `(#:port ,%www-listen-port
                            #:addr ,INADDR_ANY)))))
        (primitive-exit))))
