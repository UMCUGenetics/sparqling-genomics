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

(define-module (auth-manager config)
  #:use-module (oop goops)
  #:use-module (www util)
  #:export (%user-agent

            fork-on-startup?
            set-fork-on-startup!

            developer-mode?
            set-developer-mode!

            set-www-upload-root!
            www-upload-root

            www-cache-root

            www-listen-address-family
            www-listen-address
            set-www-listen-address!

            www-listen-port
            set-www-listen-port!

            www-max-file-size
            set-www-max-file-size!

            www-name
            set-www-name!

            sg-web-uri
            set-sg-web-uri!

            self-uri
            set-self-uri!

            rdf-store-uri
            set-rdf-store-uri!

            rdf-store-backend
            set-rdf-store-backend!

            isql-bin
            set-isql-bin!
            isql-hostname
            set-isql-hostname!
            isql-port
            set-isql-port!
            rdf-store-username
            set-rdf-store-username!
            rdf-store-password
            set-rdf-store-password!))

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

  (www-name                 #:init-value (gethostname)
                            #:getter get-www-name
                            #:setter set-www-name-private!)

  (www-self-uri               #:init-value '()
                            #:getter get-www-self-uri
                            #:setter set-www-self-uri-private!)

  (www-upload-root          #:init-value
                            (lambda _
                              (if (getenv "SG_WEB_UPLOAD_ROOT")
                                  (getenv "SG_WEB_UPLOAD_ROOT")
                                  (if (getenv "TMPDIR")
                                      (getenv "TMPDIR")
                                      "/tmp")))
                            #:setter set-www-upload-root-private!
                            #:getter get-www-upload-root)

  (www-cache-root           #:init-value
                            (lambda _
                              (let ((xdg-cache-home (getenv "XDG_CACHE_HOME")))
                                (if xdg-cache-home
                                    (string-append xdg-cache-home
                                                   "/sparqling-genomics")
                                    (string-append
                                     (getenv "HOME")
                                     "/.cache/sparqling-genomics"))))
                            #:getter get-www-cache-root)

  (www-max-file-size        #:init-value 250000000
                            #:getter get-www-max-file-size)

  (www-listen-address-family #:init-value AF_INET
                            #:getter get-www-listen-address-family
                            #:setter set-www-listen-address-family-private!)

  (www-listen-address       #:init-value INADDR_LOOPBACK
                            #:getter get-www-listen-address
                            #:setter set-www-listen-address-private!)

  (www-listen-port          #:init-value 5000
                            #:getter get-www-listen-port
                            #:setter set-www-listen-port-private!)

  ;; Other settings
  ;; --------------------------------------------------------------------------
  (fork-on-startup?         #:init-value #f
                            #:getter get-fork-on-startup?
                            #:setter set-fork-on-startup-private!)

  (developer-mode?          #:init-value #f
                            #:getter get-developer-mode?
                            #:setter set-developer-mode-private!)

  ;; sg-web configuration
  ;; --------------------------------------------------------------------------
  (sg-web-uri               #:init-value '()
                            #:getter get-sg-web-uri
                            #:setter set-sg-web-uri-private!)

  ;; RDF-store configuration
  ;; --------------------------------------------------------------------------
  (rdf-store-uri            #:init-value #f
                            #:getter get-rdf-store-uri
                            #:setter set-rdf-store-uri-private!)

  (rdf-store-backend        #:init-value #f
                            #:getter get-rdf-store-backend
                            #:setter set-rdf-store-backend-private!)

  (rdf-store-username       #:init-value #f
                            #:getter get-rdf-store-username
                            #:setter set-rdf-store-username-private!)

  (rdf-store-password       #:init-value #f
                            #:getter get-rdf-store-password
                            #:setter set-rdf-store-password-private!)

  ;; Virtuoso-specifics
  ;; --------------------------------------------------------------------------
  (isql-bin                 #:init-value #f
                            #:getter get-isql-bin
                            #:setter set-isql-bin-private!)

  (isql-hostname            #:init-value #f
                            #:getter get-isql-hostname
                            #:setter set-isql-hostname-private!)

  (isql-port                #:init-value #f
                            #:getter get-isql-port
                            #:setter set-isql-port-private!))


;; Create an instance of the <runtime-configuration> environment.
;; ----------------------------------------------------------------------------
(define %am-runtime-configuration (make <runtime-configuration>))

(define %user-agent "SPARQLing-genomics Authentication Manager")

;; ----------------------------------------------------------------------------
;; CONVENIENCE FUNCTIONS
;; ----------------------------------------------------------------------------
;;
;; These functions abstract away the need to know about the implementation
;; details.  Each property of <runtime-configuration> can be accessed using a
;; parameterless function.
;;

(define-syntax-rule
  (www-upload-root)
  (get-www-upload-root %am-runtime-configuration))

(define-syntax-rule
  (set-www-upload-root! arg)
  (set-www-upload-root-private! %am-runtime-configuration arg))

(define-syntax-rule
  (www-name)
  (get-www-name %am-runtime-configuration))

(define-syntax-rule
  (set-www-name! arg)
  (set-www-name-private! %am-runtime-configuration arg))

(define (www-cache-root)
  (let ((cache-root (get-www-cache-root %am-runtime-configuration)))
    (unless (file-exists? (cache-root))
      (mkdir-p (cache-root)))
    (cache-root)))

(define-syntax-rule
  (www-max-file-size)
  (get-www-max-file-size %am-runtime-configuration))

(define-syntax-rule
  (www-listen-address-family)
  (get-www-listen-address-family %am-runtime-configuration))

(define-syntax-rule
  (www-listen-address)
  (get-www-listen-address %am-runtime-configuration))

(define-syntax-rule
  (www-listen-port)
  (get-www-listen-port %am-runtime-configuration))

(define-syntax-rule
  (fork-on-startup?)
  (get-fork-on-startup? %am-runtime-configuration))

(define-syntax-rule
  (developer-mode?)
  (get-developer-mode? %am-runtime-configuration))

(define-syntax-rule
  (set-fork-on-startup! arg)
  (set-fork-on-startup-private! %am-runtime-configuration arg))

(define-syntax-rule
  (isql-bin)
  (get-isql-bin %am-runtime-configuration))

(define-syntax-rule
  (isql-hostname)
  (get-isql-hostname %am-runtime-configuration))

(define-syntax-rule
  (isql-port)
  (get-isql-port %am-runtime-configuration))

(define-syntax-rule
  (rdf-store-username)
  (get-rdf-store-username %am-runtime-configuration))

(define-syntax-rule
  (rdf-store-password)
  (get-rdf-store-password %am-runtime-configuration))

(define-syntax-rule
  (set-isql-bin! arg)
  (set-isql-bin-private! %am-runtime-configuration arg))

(define-syntax-rule
  (set-isql-hostname! arg)
  (set-isql-hostname-private! %am-runtime-configuration arg))

(define-syntax-rule
  (set-isql-port! arg)
  (set-isql-port-private! %am-runtime-configuration arg))

(define-syntax-rule
  (set-rdf-store-username! arg)
  (set-rdf-store-username-private! %am-runtime-configuration arg))

(define-syntax-rule
  (set-rdf-store-password! arg)
  (set-rdf-store-password-private! %am-runtime-configuration arg))

(define (set-www-listen-address! arg)
  (if (string? arg)
      (set-www-listen-address-private!
       %am-runtime-configuration
       (cond
        [(string= arg "INADDR_ANY")       INADDR_ANY]
        [(string= arg "INADDR_LOOPBACK")  INADDR_LOOPBACK]
        [(string-contains arg ":")
         (begin
           (set-www-listen-address-family-private!
            %am-runtime-configuration AF_INET6)
           (inet-pton AF_INET6 arg))]
        [(string-contains arg ".")
         (begin
           (set-www-listen-address-family-private!
            %am-runtime-configuration AF_INET)
           (inet-pton AF_INET arg))]))
      #f))

(define-syntax-rule
  (set-www-listen-port! arg)
  (set-www-listen-port-private! %am-runtime-configuration arg))

(define-syntax-rule
  (set-self-uri! arg)
  (set-www-self-uri-private! %am-runtime-configuration arg))

(define-syntax-rule
  (self-uri)
  (get-www-self-uri %am-runtime-configuration))

(define-syntax-rule
  (set-sg-web-uri! arg)
  (set-sg-web-uri-private! %am-runtime-configuration arg))

(define-syntax-rule
  (sg-web-uri)
  (get-sg-web-uri %am-runtime-configuration))

(define-syntax-rule
  (set-rdf-store-uri! arg)
  (set-rdf-store-uri-private! %am-runtime-configuration arg))

(define-syntax-rule
  (rdf-store-uri)
  (get-rdf-store-uri %am-runtime-configuration))

(define-syntax-rule
  (set-rdf-store-backend! arg)
  (set-rdf-store-backend-private! %am-runtime-configuration arg))

(define-syntax-rule
  (rdf-store-backend)
  (get-rdf-store-backend %am-runtime-configuration))
