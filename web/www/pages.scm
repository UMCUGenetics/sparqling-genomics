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

(define-module (www pages)
  #:use-module (srfi srfi-1)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:export (page-root-template
            page-empty-template))

(define page-title-prefix (string-append (www-name) " | "))

(define (page-is-ontology? request-path) #f)

(define pages
  '(("/" "Overview")
    ("/connections" "Connections")
    ("/projects" "Projects")
    ("/query" "Query")
    ("/getting-started" "Getting started")
    ("/help" "Help")
    ("/logout" "Logout")))

(define (page-partial-main-menu request-path)
  (let ((page-is-ontology (page-is-ontology? request-path)))
    `(ul
      ,(map
        (lambda (item)
          (cond
           ((string= (substring (car item) 1) (car (string-split (substring request-path 1) #\/)))
            `(li (@ (class "active")) ,(cadr item)))
           ((and (string= "query" (car (string-split (substring request-path 1) #\/)))
                 (string= (car item) "/query"))
            `(li (@ (class "active")) (a (@ (href "/query")) "← New query")))
           ((and page-is-ontology
                 (string= (car item) "/query"))
            `(li (@ (class "active")) (a (@ (href "/query")
                                            (onclick "history.go(-1); return false;")) "← Go back")))
           ((or (and (string= (car item) "/connections")
                     (string-prefix? "/edit-connection" request-path))
                (and (string= (car item) "/projects")
                     (string-prefix? "/edit-project" request-path)))
            `(li (@ (class "active")) (a (@ (href ,(car item))) "← Go back")))
           (else
            `(li (a (@ (href ,(car item))) ,(cadr item))))))
        pages))))

(define* (page-root-template title request-path content-tree #:key (dependencies '(test)))
  `((html (@ (lang "en"))
     (head
      (title ,(string-append page-title-prefix title))
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
      (link (@ (rel "icon")
               (type "image/x-icon")
               (href "/static/favicon.ico")))
      ,(if (memq 'jquery dependencies)
           `(script (@ (type "text/javascript") (src "/static/jquery-3.2.1.min.js")) "")
           `())
      ,(if (memq 'datatables dependencies)
           `((link (@ (rel "stylesheet") (type "text/css") (href "/static/datatables.min.css")))
             (script (@ (type "text/javascript") (src "/static/jquery.dataTables.min.js")) ""))
           `())
      ,(if (memq 'ace dependencies)
           `((script (@ (type "text/javascript") (charset "utf-8") (src "/static/ace/ace.js")) "")
             (script (@ (type "text/javascript") (charset "utf-8") (src "/static/ace/ext-language_tools.js")) ""))
           `())
      (link (@ (rel "stylesheet")
               (href "/static/css/main.css")
               (type "text/css")
               (media "screen"))))
     (body
      (div (@ (id "wrapper"))
           (div (@ (id "header"))
                (div (@ (class "title")
                        (style "text-align: center"))
                     (img (@ (src "/static/images/logo.png")
                             (alt ,(www-name)))))
                (div (@ (class "menu"))
                     ,(page-partial-main-menu request-path)))
           (div (@ (id "content"))
                ,content-tree)
           (div (@ (id "footer"))
                (p "v" ,(www-version) " | "
                   (a (@ (href "https://github.com/UMCUgenetics/sparqling-genomics"))
                      "Download the source code of this page."))))))))

(define* (page-empty-template title request-path content-tree #:key (dependencies '(test)))
  `((html (@ (lang "en"))
     (head
      (title ,(string-append page-title-prefix title))
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
      (link (@ (rel "icon")
               (type "image/x-icon")
               (href "/static/favicon.ico")))
      (link (@ (rel "stylesheet")
               (href "/static/css/main.css")
               (type "text/css")
               (media "screen"))))
     (body
      (div (@ (id "wrapper"))
           (div (@ (id "header"))
                (div (@ (class "title")
                        (style "text-align: center"))
                     (img (@ (src "/static/images/logo.png")
                             (alt ,(www-name))))))
           (div (@ (id "content"))
                ,content-tree)
           (div (@ (id "footer"))
                (p "v" ,(www-version) " | "
                   (a (@ (href "https://github.com/UMCUgenetics/sparqling-genomics"))
                      "Download the source code of this page."))))))))
