;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
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

(define-module (auth-manager permission-check)
  #:use-module (auth-manager config)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (sparql parser)
  #:use-module (srfi srfi-1)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (logger)

  #:export (has-unscoped-variables?
            may-execute?
            token->user))

(define (token->user token)
  "Resolves TOKEN to a username, or #f if the token is invalid."
  (catch #t
    (lambda _
      (if (string? token)
          (receive (header port)
              (http-get (string-append (sg-web-uri) "/api/user-info")
                        #:streaming? #t
                        #:headers `((user-agent . ,%user-agent)
                                    (Cookie . ,token)
                                    (accept . ((application/s-expression)))))
            (cond
             [(= (response-code header) 200)
              (let [(data (read port))]
                (close-port port)
                (assoc-ref data 'username))]
             [else
              #f]))
          #f))
    (lambda (key . args)
      (log-error "token->user" "~a: ~a" key args)
      #f)))

(define (graphs-by-project auth-token project-id)
  "Returns a list of graphs part of PROJECT-ID that may be accessed using
AUTH-TOKEN."
  (let ((uri (string-append (sg-web-uri) "/api/graphs-by-project")))
    (receive (header port)
        (http-post uri
                   #:headers `((user-agent   . ,%user-agent)
                               (Cookie       . ,auth-token)
                               (content-type . (application/s-expression))
                               (accept       . ((application/s-expression))))
                   #:body    (call-with-output-string
                               (lambda (port)
                                 (write `((project-id . ,project-id)
                                          (connection . ,(www-name))) port)))
                   #:streaming? #t)
      (if (= (response-code header) 200)
          (let ((output (read port)))
            (close-port port)
            output)
          (begin
            (log-debug "graphs-by-project" "Reading graphs-by-project failed.")
            (close-port port)
            '())))))

(define (inferred-graphs query)
  "Returns a list of graph names that are used in the query."
  (append (query-global-graphs query)
          (delete #f (map quint-graph (query-quints query)))))

(define (has-unscoped-variables? query)
  "Returns #t when there is a triplet pattern without an explicit graph,
otherwise it returns #f."
  (any not (map quint-graph (query-quints query))))
(define (lock-check triplets graphs-in-project)
  (let* ((graphs       (map quint-graph triplets))
         (graph-states (map (lambda (graph)
                               `(,(assoc-ref graph "graph") .
                                 ,(assoc-ref graph "isLocked")))
                             graphs-in-project))
         (lock-state    (delete #f
                         (map (lambda (graph)
                                (let ((state (assoc-ref graph-states graph)))
                                  (if (or (not state)
                                          (string= state "1"))
                                      graph
                                      #f)))
                              graphs))))
    (if (null? lock-state)
        (values #t "")
        (values #f (format #f "The following graphs are locked:~{~%-> ~a~}"
                           lock-state)))))

(define (may-execute? auth-token project-id query)
  "Returns #t when the query may be executed, #f otherwise."
  (let [(parsed (if (is-a? query <query>)
                    query
                    (parse-query query)))]

    ;; The parser must be able to parse the query.
    ;; -----------------------------------------------------------------------
    (if (not parsed)
        (values #f (format #f "Couldn't parse:~%~a" query))
        (let* [(allowed-graphs (map (lambda (item) (assoc-ref item "graph"))
                                    (graphs-by-project auth-token project-id)))
               (global-graphs  (query-global-graphs parsed))
               (disallowed-graphs (lset-difference string= global-graphs
                                                   allowed-graphs))
               (used-graphs    (inferred-graphs parsed))]
          (cond
           ;; Check whether all variables are scoped in a graph.
           ;; -----------------------------------------------------------------
           [(or  (and (has-unscoped-variables? parsed)
                      (null? global-graphs))
                 (and (has-unscoped-variables? parsed)
                      (not (null? disallowed-graphs))))
            (if (not (null? disallowed-graphs))
                (values #f (format #f "Disallowed graphs:~{~%-> ~a~}"
                                   disallowed-graphs))
                (values #f
                 (string-append
                  "Specify the graph to search for the following triplets:"
                  (format #f "~{~%-> ~a~}"
                    (delete #f (map (lambda (quint)
                                      (if (quint-graph quint) #f
                                          (format #f "~a ~a ~a"
                                                  (quint-subject quint)
                                                  (quint-predicate quint)
                                                  (quint-object quint))))
                                    (query-quints parsed)))))))]

           ;; Check whether only allowed-graphs are used.
           ;; -----------------------------------------------------------------
           [(not (null? (lset-difference string= used-graphs allowed-graphs)))
            (let* [(g (delete-duplicates
                       (lset-difference string= used-graphs allowed-graphs)))]
              (values #f (format #f "Disallowed graphs:~{~%-> ~a~}" g)))]

           ;; Check whether INSERT or DELETE operations are done on unlocked
           ;; graphs only.
           ;; -----------------------------------------------------------------
           [(eq? (query-type parsed) 'INSERT)
            (lock-check (query-insert-patterns parsed) graphs-in-project)]

           [(eq? (query-type parsed) 'DELETE)
            (lock-check (query-delete-patterns parsed) graphs-in-project)]

           [(eq? (query-type parsed) 'DELETEINSERT)
            (let ((check-graphs (append (query-insert-patterns parsed)
                                        (query-delete-patterns parsed))))
              (lock-check check-graphs graphs-in-project))]

           ;; If all previous tests passed, the query may be executed.
           ;; -----------------------------------------------------------------
           [else
            (log-debug "may-execute?" "Approved:~%---~%~a~%---" query)
            (values #t "")])))))
