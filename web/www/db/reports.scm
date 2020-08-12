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

(define-module (www db reports)
  #:use-module (ice-9 ftw)
  #:use-module (logger)
  #:use-module (srfi srfi-1)
  #:use-module (www config)
  #:use-module (www util)

  #:export (report-friendly-name
            resolve-report-module
            report-modules
            reports-for-project))

(define (resolve-report-module request-path)
  (let* ((relative-path (if (eq? (string-ref request-path 0) #\/)
                             (substring request-path 1)
                             request-path))
         (full-path     (string-split relative-path #\/))
         (module-path   `(www reports ,(string->symbol (car full-path))))
         (module        (if (developer-mode?)
                            (catch 'wrong-type-arg
                              (lambda _
                                (reload-module
                                 (resolve-module module-path #:ensure #f)))
                              (lambda (key . args) #f))
                            (resolve-module module-path #:ensure #f))))

    ;; Return #f unless the expected symbols exist in 'module',
    ;; in which case we return that.
    (if module
        (catch #t
          (lambda _ `((module          . ,module-path)
                      (title           . ,(module-ref module 'title))
                      (project         . ,(module-ref module 'project))
                      (report-overview . ,(module-ref module 'report-overview))))
          (lambda (key . args)
            (log-error "resolve-report-module"
                       "Couldn't resolve the module's structure for ~s."
                       request-path)
            (log-error "resolve-report-module" "~a: ~s" key args)
            #f))
        (begin
          (log-error "resolve-report-module"
                     "Couldn't resolve module for ~s." request-path)
          #f))))

(define (report-module-from-path path)
  (resolve-report-module (if (eq? (string-ref path 0) #\/)
                             (substring path 1)
                             path)))

(define (all-report-paths)
  (map (lambda (file)
         (basename file ".scm"))
       (flatten
        (delete #f (map (lambda (path)
                          (scandir (string-append path "/www/reports")
                                   (lambda (file)
                                     (string-suffix? ".scm" file))))
                        %load-path)))))

(define report-modules
  ;; Modules are typically loaded only once, so we can keep a cache here.
  (let ((%all-reports-cache #f))
    (lambda ()
      (if (and (not (developer-mode?))
               (not %all-reports-cache))
          (let ((reports (all-report-paths)))
            (if (null? reports)
                (set! %all-reports-cache '())
                (set! %all-reports-cache (map report-module-from-path reports)))
            (report-modules))
          %all-reports-cache))))

(define (reports-for-project project-id)
  (delete #f
   (map (lambda (module)
          (if (and (assoc-ref module 'project)
                   (string= (assoc-ref module 'project) project-id))
              module
              #f))
        (report-modules))))

(define (report-friendly-name module)
  (if module
      (assoc-ref module 'module)
      #f))
