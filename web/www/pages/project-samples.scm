(define-module (www pages project-samples)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db projects)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (json)
  #:export (page-project-samples))

(define* (page-project-samples request-path #:optional (format 'json)
                                            #:key (post-data ""))
  (let ((project (project-by-name request-path)))
    (cond
     [(eq? format 'json)
      (scm->json-string (project-samples project))]
     [(eq? format 'ntriples)
      (project->ntriples project)])))