;;; Copyright © 2016, 2017, 2018  Roel Janssen <roel@gnu.org>
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

(define-module (www config)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module ((www util) #:select (mkdir-p))
  #:export (default-uri-strings
            fork-on-startup?
            initialize-internal-rdf
            set-fork-on-startup!
            set-www-listen-port!
            graph-name-max-length
            uri-string
            www-listen-port
            www-max-file-size
            www-name
            www-hostname
            www-root
            www-cache-root
            www-static-root))


;; ----------------------------------------------------------------------------
;; RUNTIME-CONFIGURATION CLASS
;; ----------------------------------------------------------------------------
;;
;; This class definition collects all run-time configurable options plus a few
;; “basic infrastructure” properties.
;;

(define-class <runtime-configuration> ()

  ;; Web settings
  ;; --------------------------------------------------------------------------
  (www-name                #:init-value "sparqling-genomics"
                           #:getter get-www-name)

  (www-hostname            #:init-value "http://sparqling-genomics/"
                           #:getter get-www-hostname)

  (www-root                #:init-value
                           (lambda _
                             (if (getenv "SG_WEB_ROOT")
                                 (getenv "SG_WEB_ROOT")
                                 (dirname (search-path %load-path "sg-web"))))
                           #:getter get-www-root)
  (www-cache-root          #:init-value
                           (lambda _
                             (let ((xdg-cache-home (getenv "XDG_CACHE_HOME")))
                               (if xdg-cache-home
                                   (string-append xdg-cache-home
                                                  "/sparqling-genomics")
                                   (string-append
                                    (getenv "HOME")
                                    "/.cache/sparqling-genomics"))))
                           #:getter get-www-cache-root)
  (www-static-root         #:init-value
                           (lambda _
                             (if (getenv "SG_WEB_ROOT")
                                 (string-append (getenv "SG_WEB_ROOT") "/static")
                                 (string-append
                                  (dirname (search-path %load-path "sg-web"))
                                  "/static")))
                           #:getter get-www-static-root)

  (www-max-file-size       #:init-value 250000000
                           #:getter get-www-max-file-size)

  (www-listen-port         #:init-value 5000
                           #:getter get-www-listen-port
                           #:setter set-www-listen-port-private!)

  ;; Other settings
  ;; --------------------------------------------------------------------------
  (fork-on-startup?        #:init-value #f
                           #:getter get-fork-on-startup?
                           #:setter set-fork-on-startup-private!)

  (graph-name-max-length   #:init-value 128
                           #:getter get-graph-name-max-length))


;; Create an instance of the <runtime-configuration> environment.
;; ----------------------------------------------------------------------------
(define %runtime-configuration (make <runtime-configuration>))


;; ----------------------------------------------------------------------------
;; CONVENIENCE FUNCTIONS
;; ----------------------------------------------------------------------------
;;
;; These functions abstract away the need to know about the implementation
;; details.  Each property of <runtime-configuration> can be accessed using a
;; parameterless function.
;;

(define-syntax-rule
  (www-name)
  (get-www-name %runtime-configuration))

(define-syntax-rule
  (www-hostname)
  (get-www-hostname %runtime-configuration))

(define-syntax-rule
  (www-root)
  ((get-www-root %runtime-configuration)))

(define (www-cache-root)
  (let ((cache-root (get-www-cache-root %runtime-configuration)))
    (unless (file-exists? (cache-root))
      (mkdir-p (cache-root)))
    (cache-root)))

(define-syntax-rule
  (www-static-root)
  ((get-www-static-root %runtime-configuration)))

(define-syntax-rule
  (www-max-file-size)
  (get-www-max-file-size %runtime-configuration))

(define-syntax-rule
  (www-listen-port)
  (get-www-listen-port %runtime-configuration))

(define-syntax-rule
  (fork-on-startup?)
  (get-fork-on-startup? %runtime-configuration))

(define-syntax-rule
  (graph-name-max-length)
  (get-graph-name-max-length %runtime-configuration))

(define-syntax-rule
  (set-fork-on-startup! arg)
  (set-fork-on-startup-private! %runtime-configuration arg))

(define-syntax-rule
  (set-www-listen-port! arg)
  (set-www-listen-port-private! %runtime-configuration arg))

;; ----------------------------------------------------------------------------
;; DEFAULT URIS
;; ----------------------------------------------------------------------------
;;
;; The following symbol collects commonly used URI strings.
;;

(define default-uri-strings
  '((rdf          . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rdfs         . "http://www.w3.org/2000/01/rdf-schema#")
    (xsd          . "http://www.w3.org/2001/XMLSchema#")
    (owl          . "http://www.w3.org/2002/07/owl#")
    (dc           . "http://purl.org/dc/elements/1.1/")
    (internal     . "http://sparqling-genomics/")))

(define-syntax-rule
  (uri-string key)
  (assoc-ref default-uri-strings key))
