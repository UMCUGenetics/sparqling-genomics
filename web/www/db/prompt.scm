;;; Copyright © 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www db prompt)
  #:use-module (ice-9 receive)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (srfi srfi-1)
  #:use-module (web response)
  #:use-module (www config)
  #:use-module (www db connections)
  #:use-module (www db exploratory)
  #:use-module (www db sessions)
  #:use-module (www util)
  #:use-module (logger)

  #:export (prompt-insert-triplet
            prompt-remove-triplet
            prompt-get-triplets
            prompt-clear-triplets
            prompt-save-session))

(define (restore-uri input)
  (cond
   [(is-uri? input)
    (if (string-prefix? "<" input)
        input
        (string-append "<" input ">"))]
   [(is-shorthand-uri? input)
    input]
   [(string->number input)
    input]
   [else
    (if (string-prefix? "\"" input)
        input
        (string-append "\"" input "\""))]))

;; PROMPT-INSERT-TRIPLET
;; ----------------------------------------------------------------------------
;;
;; This function stores a value into the cache graph.
;;

(define (prompt-insert-triplet username subject predicate object)
  (let [(connection (default-connection username))]
    (catch #t
      (lambda _
        (let [(query (format #f "~a
INSERT INTO <http://~a/sg-prompt> {
  ~a ~a ~a
}" default-prefixes username
 (restore-uri subject)
 (restore-uri predicate)
 (restore-uri object)))]
          (receive (header port)
              (sparql-query query
                            #:uri (connection-uri connection)
                            #:store-backend
                            (connection-backend connection)
                            #:digest-auth
                            (if (and (connection-username connection)
                                     (connection-password connection))
                                (string-append
                                 (connection-username connection) ":"
                                 (connection-password connection))
                                #f))
            (= (response-code header) 200))))
      (lambda (key . args)
        (log-error "prompt-insert-triplet"
                   "Unknown exception thrown: ~a: ~a~%"
                   key args)))))

;; PROMPT-REMOVE-TRIPLET
;; ----------------------------------------------------------------------------
;;
;; This function removes a triplet from the prompt session's graph.
;;

(define (prompt-remove-triplet username subject predicate object)
  (let [(connection (default-connection username))]
    (catch #t
      (lambda _
        (let [(query (format #f "~a
DELETE DATA {
  GRAPH <http://~a/sg-prompt> {
    ~a ~a ~a
  }
}
" default-prefixes
  username
  (restore-uri subject)
  (restore-uri predicate)
  (restore-uri object)))]
          (receive (header port)
              (user-sparql-query username query)
            (= (response-code header) 200))))
      (lambda (key . args)
        (log-error "prompt-remove-triplet"
                   "Unknown exception thrown ~a: ~a~%"
                   key args)))))

;; PROMPT-GET-TRIPLETS
;; ----------------------------------------------------------------------------

(define (prompt-get-triplets username)
  (let [(connection (default-connection username))]
    (catch #t
      (lambda _
        (let [(results
               (query-results->list
                (sparql-query
                 (format #f "SELECT DISTINCT ?s ?p ?o WHERE { GRAPH <~a> { ?s ?p ?o } }"
                         (string-append "http://" username "/sg-prompt"))
                 #:uri           (connection-uri connection)
                 #:store-backend (connection-backend connection)
                 #:digest-auth   (if (and (connection-username connection)
                                          (connection-password connection))
                                     (string-append
                                      (connection-username connection) ":"
                                      (connection-password connection))
                                     #f))
                #t))]
          (if results
              results
              '())))
      (lambda (key . args)
        (log-error "prompt-get-triplet"
                   "Unknown exception thrown ~a: ~a~%"
                   key args)
        '()))))

;; PROMPT-CLEAR-TRIPLETS
;; ----------------------------------------------------------------------------

(define (prompt-clear-triplets username)
  (let [(connection (default-connection username))]
    (catch #t
      (lambda _
        (query-results->list
         (sparql-query
          (string-append "CLEAR GRAPH <http://" username "/sg-prompt>")
          #:uri           (connection-uri connection)
          #:store-backend (connection-backend connection)
          #:digest-auth   (if (and (connection-username connection)
                                   (connection-password connection))
                              (string-append
                               (connection-username connection) ":"
                               (connection-password connection))
                              #f))
         #t))
      (lambda (key . args)
        (log-error "prompt-clear-triplets"
                   "Unknown exception thrown ~a: ~a~%"
                   key args)
        '()))))

(define (prompt-save-session username graph)
  (let [(connection (default-connection username))]
    (catch #t
      (lambda _
        (receive (header port)
            (sparql-query
             (string-append "INSERT { GRAPH <" graph "> { ?s ?p ?o } } "
                            "WHERE { GRAPH <http://" username "/sg-prompt> { "
                            "?s ?p ?o } }")
             #:uri           (connection-uri connection)
             #:store-backend (connection-backend connection)
             #:digest-auth   (if (and (connection-username connection)
                                      (connection-password connection))
                                 (string-append
                                  (connection-username connection) ":"
                                  (connection-password connection))
                                 #f))
          (if (= (response-code header) 200)
              (prompt-clear-triplets username)
              #f)))
      (lambda (key . args)
        (log-error "prompt-save-session"
                   "Unknown exception thrown ~a: ~a~%"
                   key args)
        '()))))
