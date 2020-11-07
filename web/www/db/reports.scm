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
  #:use-module (www hashing)
  #:use-module (www reports)
  #:use-module (www util)

  #:export (report-friendly-name
            resolve-report-module
            report-modules
            reports-for-project
            report-for-project-by-name

            r-sweave-reports-for-project
            r-sweave-report-by-hash
            r-sweave-report-pdf))

;; ----------------------------------------------------------------------------
;; SCHEME MODULES REPORTING
;; ----------------------------------------------------------------------------

(define (resolve-report-module request-path)
  (let* ((relative-path (if (eq? (string-ref request-path 0) #\/)
                             (substring request-path 1)
                             request-path))
         (full-path     (string-split relative-path #\/))
         (module-name   (string->symbol (car full-path)))
         (module-path   `(www reports ,module-name))
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
                      (name            . ,module-name)
                      (title           . ,(module-ref module 'title))
                      (project         . ,(module-ref module 'project))
                      (report-overview . ,(module-ref module 'report-overview))
                      (report-pdf      . ,(module-ref module 'report-pdf))))
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

(define (report-for-project-by-name project-id name)
  (catch #t
    (lambda _
      (car (delete #f
            (map (lambda (module)
                   (if (and (assoc-ref module 'name)
                            (eq? (assoc-ref module 'name)
                                 (string->symbol name)))
                       module
                       #f))
                 (reports-for-project project-id)))))
    (lambda (key . args) #f)))

(define (report-friendly-name module)
  (if module
      (assoc-ref module 'module)
      #f))

;; ----------------------------------------------------------------------------
;; R SWEAVE REPORTING
;; ----------------------------------------------------------------------------

(define (r-sweave-reports-for-project project-id)
  (let* ((rnw-dir (string-append (r-reports-roots) "/" project-id))
         (entries (scandir rnw-dir (lambda (file)
                                     (string-suffix? ".Rnw" file)))))
    (if entries
        (map (lambda (file)
               (let ((full-path (string-append rnw-dir "/" file)))
                 `((md5      . ,(md5sum-from-file full-path))
                   (filename . ,full-path))))
             (delete #f entries))
        '())))

(define (r-sweave-report-by-hash project-id hash)
  (let* ((reports (r-sweave-reports-for-project project-id))
         (matches (delete #f
                    (map (lambda (report)
                           (if (string= (assoc-ref report 'md5) hash)
                               report
                               #f))
                         reports))))
    (if (= (length matches))
        (car matches)
        #f)))

(define* (r-sweave-report-pdf project-id filename #:key (refresh? #f))
  (let* ((cache-dir (string-append (r-reports-roots) "/r-reports/" project-id))
         (hash      (md5sum-from-file filename))
         (tex       (string-append cache-dir "/" hash ".tex"))
         (pdf       (string-append cache-dir "/" hash ".pdf")))

    ;; Ensure the cache directory is created if it does not exist.
    (mkdir-p cache-dir)

    ;; Serve the report.
    (cond
     [(and (not refresh?)
           (file-exists? pdf))       pdf]
     [(r-sweave-report filename tex) pdf]
     [else                           #f])))
