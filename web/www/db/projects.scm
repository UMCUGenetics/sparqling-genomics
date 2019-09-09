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

(define-module (www db projects)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (sparql driver)
  #:use-module (sparql util)
  #:use-module (web response)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs io ports)
  #:use-module (logger)

  #:export (project-add
            project-edit
            project-remove
            project-is-active?
            active-project
            all-projects
            active-project-for-user
            active-writable-graphs-for-user
            projects-by-user
            project-by-name
            project-by-id
            project-by-hash

            alist->project
            project->alist
            project->ntriples

            make-project
            project-id
            project-hash
            project-name
            project-samples
            project-members
            project-assigned-graphs
            project-inferred-graphs
            project?
            project-exists?
            project-is-created-by?
            project-has-member?

            project-assign-graph!
            project-forget-graph!
            project-assign-member!
            project-forget-member!

            project-lock-assigned-graph!
            project-unlock-assigned-graph!

            set-project-name!
            set-project-samples!
            set-active-project-for-user!))

;; PROJECT
;; ----------------------------------------------------------------------------
;;
;; Projects are symbolic glue for data, queries, and agents.  The following
;; functions implement interactions with projects.
;;

;; BASE SELECT QUERY
;; ----------------------------------------------------------------------------

(define (generate-query-with-filters filters)
  (string-append
   default-prefixes
   "
SELECT ?project AS ?projectId ?creator ?name ?date
FROM <" system-state-graph ">
WHERE {
  ?project rdf:type sg:Project .
  OPTIONAL {
    ?project dcterms:creator ?creator ;
             dcterms:title   ?name ;
             dcterms:date    ?date .
    ?agent   sg:isAssignedTo ?project .
  }
"
   (if filters
       (format #f "~{  FILTER (~a)~%~}" filters)
       "")
   "}"))

;; SIMPLE GETTERS
;; ----------------------------------------------------------------------------

(define-syntax-rule (project-id project)         (assoc-ref project "projectId"))
(define-syntax-rule (project-name project)       (assoc-ref project "name"))
(define-syntax-rule (project-creator project)    (assoc-ref project "creator"))
(define-syntax-rule (project-created-at project) (assoc-ref project "createdAt"))

(define (project-hash project)
  (basename (project-id project)))

;; PROJECTS PERSISTENCE
;; ----------------------------------------------------------------------------
(define (persist-project username name)

  (let* [(project-id (generate-id username name))
         (timestamp (strftime "%Y-%m-%d %H:%M:%S" (gmtime (current-time))))
         (query (string-append
                 default-prefixes
                 "INSERT INTO <" system-state-graph "> { "
                 "project:" project-id
                 " rdf:type sg:Project ;"
                 " dcterms:creator agent:" username " ;"
                 " dcterms:title " (format #f "~s^^xsd:string" name) " ;"
                 " dcterms:date \"" timestamp "\"^^xsd:dateTime ."
                 " agent:" username " sg:isAssignedTo project:" project-id " ."
                 "}"))
         (connection (system-connection))]
    (receive (header body)
        (system-sparql-query query)
      (if (= (response-code header) 200)
          (begin
            (set-active-project-for-user!
             username
             (string-append
              (assoc-ref default-uri-strings 'project)
              project-id))
            #t)
          (begin
            (display (get-string-all body))
            #f)))))

;; PROJECT-ADD
;; ----------------------------------------------------------------------------
(define (project-add name username)
  "Adds a project to the system state."
  (cond
   [(string-is-longer-than name (graph-name-max-length))
    (values #f (format #f "The project name cannot be longer than ~a characters."
                       (graph-name-max-length)))]
   [(string= name "")
    (values #f (format #f "An empty project name is not allowed."))]
   [(project-exists? name username)
    (values #f (format #f "There already exists a project with this name."))]
   [#t (if (persist-project username name)
           (values #t "The project has been added.")
           (values #f "The project could not be added."))]))

;; PROJECT-REMOVE
;; ----------------------------------------------------------------------------
(define (project-remove project-uri username)
  "Removes the reference in the internal graph for PROJECT."
  (if (string= project-uri (active-project-for-user username))
      (values #f "Cannot remove active project.")
      ;; TODO: Should we also remove the queries related to a project?
      (let [(query (string-append
                    default-prefixes
                    "WITH <" system-state-graph ">
DELETE { <" project-uri "> ?predicate ?object . }
WHERE  { <" project-uri "> ?predicate ?object . }"))
            (connection (system-connection))]
        (receive (header body)
            (system-sparql-query query)
          (if (= (response-code header) 200)
              (values #t "")
              (values #f
                      (format #f "Could not remove project (error code ~a)"
                              (response-code header))))))))

(define (set-project-property! project-id predicate object type)
  (let [(query (string-append
                default-prefixes
                "WITH <" system-state-graph ">
DELETE { ?project " predicate " ?value . }
INSERT { ?project " predicate " " (if type
                                    (if (string= type "xsd:boolean")
                                        (if object "1" "0")
                                        (format #f "~s^^~a" object type))
                                    (format #f "<~a>" object)) " . }
WHERE  { ?project ?predicate ?value . FILTER (?project = <" project-id ">) }"))
        (connection (system-connection))]
    (receive (header body)
        (system-sparql-query query)
      (= (response-code header) 200))))

;; GET FUNCTIONS
;; ----------------------------------------------------------------------------

(define* (all-projects #:key (filter #f))
  "Returns a list of project records, applying FILTER to the records."
  (let [(results (query-results->alist
                  (system-sparql-query
                    (generate-query-with-filters '()))))]
    (if filter
        (map filter results)
        results)))

(define* (projects-by-user username #:key (filter #f))
  "Returns a list of project records, applying FILTER to the records."
  (let [(results (query-results->alist
                  (system-sparql-query
                   (generate-query-with-filters
                    (list (format #f "?agent = agent:~a" username))))))]
    (if filter
        (map filter results)
        results)))

(define (project-by-name name)
  (let [(results (query-results->alist
                  (system-sparql-query
                    (generate-query-with-filters
                     `(,(format #f "?name = ~s^^xsd:string" name))))))]
    (if (null? results)
        #f
        (car results))))

(define (project-by-id id)
  (let [(results (query-results->alist
                  (system-sparql-query
                    (generate-query-with-filters
                     `(,(format #f "?project = <~a>" id))))))]
    (if (null? results)
        #f
        (car results))))

(define (project-by-hash hash)
  (let [(results (query-results->alist
                  (system-sparql-query
                   (generate-query-with-filters
                    (list (format #f "?project = project:~a" hash))))))]
    (if (null? results)
        #f
        (car results))))

(define (project-exists? name username)
  (let* [(project-id (generate-id username name))
         (query (string-append
                 default-prefixes
                 "SELECT (COUNT(?project) AS ?projects) "
                 "FROM <" system-state-graph "> "
                 "WHERE { ?project ?p ?o . "
                 "FILTER (?project = project:" project-id ") }"))
         (results (query-results->alist (system-sparql-query query)))]
    (not (string= (assoc-ref (car results) "projects") "0"))))

(define (set-active-project-for-user! username project-id)
  (let [(delete-query (string-append
                       default-prefixes
                       "WITH <" system-state-graph ">
DELETE { agent:" username " sg:currentlyWorksOn ?project . }
WHERE  { agent:" username " sg:currentlyWorksOn ?project . }"))
        (insert-query (string-append
                       default-prefixes
                       "WITH <" system-state-graph ">
INSERT { agent:" username " sg:currentlyWorksOn <" project-id "> . }"))]
    (receive (header body) (system-sparql-query delete-query)
      (if (= (response-code header) 200)
          #t
          (begin
            (log-error "set-active-project-for-user!"
                       (get-string-all body))
            #f)))
    (receive (header body) (system-sparql-query insert-query)
      (= (response-code header) 200))))

(define (active-project-for-user username)
  (let* [(query (string-append
                 default-prefixes
                 "SELECT ?project "
                 "FROM <" system-state-graph "> "
                 "WHERE { agent:" username " sg:currentlyWorksOn ?project . }"))
         (results (query-results->alist (system-sparql-query query)))]
    (if (> (length results) 0)
        (assoc-ref (car results) "project")
        '())))

(define (active-writable-graphs-for-user username)
  (let [(query (string-append
                default-prefixes
                "SELECT ?graph FROM <http://sparqling-genomics.org/sg-web/state>
WHERE {
  ?project sg:hasAssignedGraph ?graph .
  agent:" username " sg:currentlyWorksOn ?project .

  OPTIONAL {
    ?graph sg:isLocked ?locked .
  }

  FILTER (! BOUND(?locked) OR (?locked = 0))
}"))]
    (query-results->alist (system-sparql-query query))))

;; PROJECT -> MEMBERS
;; ----------------------------------------------------------------------------
;;
;; Agents/People/Robots are the functional units of a project.  The following
;; functions implement the interactions between projects and its members.
;;

(define (project-members project-id)
  (let* [(query (string-append
                 default-prefixes
                 "SELECT DISTINCT STRAFTER(STR(?agent), STR(agent:)) AS ?user"
                 " COUNT(DISTINCT ?query) AS ?queries"
                 " FROM <" system-state-graph ">"
                 " WHERE { ?agent sg:isAssignedTo ?project ."
                 " OPTIONAL {"
                 " ?query sg:executedBy ?agent ; sg:isRelevantTo ?project ."
                 " }"
                 " FILTER (?project = <" project-id ">)"
                 " }"
                 " ORDER BY DESC(?queries)"))]
    (query-results->alist (system-sparql-query query))))

(define (project-assign-member! project-id username auth-user)
  (let [(query (string-append
                default-prefixes
                "INSERT INTO <" system-state-graph "> {"
                " agent:" username " sg:isAssignedTo ?project ."
                " } WHERE {"
                " agent:" auth-user " sg:isAssignedTo ?project ."
                " FILTER (?project = <" project-id ">) }"))]
    (receive (header body) (system-sparql-query query)
      (if (= (response-code header) 200)
          (values #t "")
          (values #f (format #f "Could not assign ~a to the project." username))))))

(define (project-has-member? project-id username)
  (let* [(query (string-append
                 default-prefixes
                 "SELECT DISTINCT ?agent"
                 " FROM <" system-state-graph ">"
                 " WHERE { ?agent sg:isAssignedTo <" project-id "> . "
                 " FILTER (?agent = agent:" username ")"
                 " }"))
         (results (query-results->alist (system-sparql-query query)))]
    (not (null? results))))

(define (project-auto-assign-authorization-for-graph project-id graph-uri)
  (let* [(query (string-append
                 default-prefixes
                 "SELECT ?auth FROM <" system-state-graph "> WHERE {"
                 " <" graph-uri "> sg:requiresAuthorization ?auth ."
                 " }"))
         (results (query-results->alist (system-sparql-query query)))]
    (if (null? results)
        (let* [(auth-id (generate-id graph-uri))
               (query (string-append
                       default-prefixes
                       "INSERT INTO <" system-state-graph "> {"
                       " <" project-id "> sg:hasAuthorization ?auth ."
                       " <" graph-uri "> sg:requiresAuthorization ?auth ."
                       " } WHERE { BIND(auth:" auth-id " AS ?auth) }"))]
          (receive (header body) (system-sparql-query query)
            (= (response-code header) 200)))
        #f)))

(define (project-forget-member! project-id username)
  (if (project-is-created-by? project-id username)
      (values #f "Cannot remove the owner of the project.")
      (let* [(query (string-append
                     default-prefixes
                     "WITH <" system-state-graph "> "
                     "DELETE {"
                     " agent:" username " sg:isAssignedTo <" project-id "> ."
                     " }"))]
        (receive (header body)
            (system-sparql-query query)
          (if (= (response-code header) 200)
              (values #t "")
              (values #f "Could not remove graph."))))))

(define (project-is-created-by? project-id username)
  (let* [(query (string-append
                 default-prefixes
                 "SELECT ?creator FROM <" system-state-graph "> WHERE {"
                 " <" project-id "> dcterms:creator ?creator ."
                 " FILTER (?creator = agent:" username ")}"))
         (results (query-results->alist (system-sparql-query query)))]
    (not (null? results))))

;; PROJECT -> GRAPH
;; ----------------------------------------------------------------------------
;;
;; Graphs contain datasets, and projects use graphs.  The following functions
;; implement the interactions between projects and graphs.
;;

(define (project-assigned-graphs project-id)
  (let* [(query (string-append
                 default-prefixes
                 "SELECT DISTINCT ?graph ?isLocked"
                 " FROM <" system-state-graph ">"
                 " WHERE { <" project-id "> sg:hasAssignedGraph ?graph ."
                 " OPTIONAL { ?graph sg:isLocked ?lockState . }"
                 " BIND(IF(BOUND(?lockState), ?lockState, \"false\"^^xsd:boolean)"
                 " AS ?isLocked)"
                 " }"))]
    (query-results->alist
     (system-sparql-query query))))

(define (project-assign-graph! project-id graph-uri username)
  ;; Attempt to authorize the project for the graph.
  ;; This only works if the graph hasn't been claimed yet.
  (project-auto-assign-authorization-for-graph project-id graph-uri)
  (let [(query (string-append
                default-prefixes
                "INSERT INTO <" system-state-graph "> {"
                " <" project-id "> sg:hasAssignedGraph <" graph-uri "> ."
                " } WHERE {"
                " <" project-id "> sg:hasAuthorization ?auth ."
                " <" graph-uri "> sg:requiresAuthorization ?auth ."
                " }"))]
    (receive (header body) (system-sparql-query query)
      (if (= (response-code header) 200)
          (values #t "")
          (values #f "The project doesn't have the required authorization.")))))

(define (project-forget-graph! project-id graph-uri)
  (let* [(query (string-append
                 default-prefixes
                 "WITH <" system-state-graph "> "
                 "DELETE { "
                 "<" project-id "> sg:hasAssignedGraph <" graph-uri "> ."
                 "}"))]
    (receive (header body)
        (system-sparql-query query)
      (if (= (response-code header) 200)
          (values #t "")
          (values #f "Could not remove graph.")))))

(define (project-lock-or-unlock-assigned-graph! project-id graph-uri lock?)
  (let* [(delete-query
          (string-append
           default-prefixes
           "WITH <" system-state-graph "> "
           "DELETE { <" graph-uri "> sg:isLocked \""
           (if lock? "false" "true") "\"^^xsd:boolean . } "
           "WHERE { <" graph-uri "> sg:isLocked \""
           (if lock? "false" "true")"\"^^xsd:boolean . }"))
         (insert-query
          (string-append
           default-prefixes
           "WITH <" system-state-graph "> "
           "INSERT { <" graph-uri ">  sg:isLocked \""
           (if lock? "true" "false") "\"^^xsd:boolean . }"))]
    (receive (header body) (system-sparql-query delete-query)
      (if (= (response-code header) 200)
          #t
          (begin
            (log-error "project-lock-or-unlock-assigned-graph!"
                       (get-string-all body))
            #f)))
    (receive (header body) (system-sparql-query insert-query)
      (if (= (response-code header) 200)
          (values #t "")
          (values #f (string-append
                      "Could not " (if lock? "lock" "unlock")
                      " the graph."))))))

(define-syntax-rule
  (project-lock-assigned-graph! project-id graph-uri)
  (project-lock-or-unlock-assigned-graph! project-id graph-uri #t))

(define-syntax-rule
  (project-unlock-assigned-graph! project-id graph-uri)
  (project-lock-or-unlock-assigned-graph! project-id graph-uri #f))

(define (project-inferred-graphs project-id)
  ;; There are three ways to scope a triplet in a quad.  The following function
  ;; is agnostic to the scope keyword.
  (define* (graphs-from-query-for-keyword query clause #:optional (graphs '()))
    (let* [(clause-length (+ (string-length clause) 2))
           (from-start (string-contains-ci query (string-append clause " <")))
           (from-end   (if from-start
                           (string-contains query ">" from-start)
                           #f))]
      (if (and from-start from-end)
          (graphs-from-query-for-keyword
           (substring query from-end) clause
           (cons
            (substring query (+ from-start clause-length) from-end) graphs))
          (reverse graphs))))

  ;; The following function extracts the three ways of scoping a triplet.
  (define (graphs-from-query query)
     (append (graphs-from-query-for-keyword query "with")
             (graphs-from-query-for-keyword query "from")
             (graphs-from-query-for-keyword query "graph")
             (graphs-from-query-for-keyword query "insert into")))

  (catch #t
    (lambda _
      (let* [(query (string-append
                     default-prefixes
                     "SELECT ?queryText"
                     " FROM <" system-state-graph ">"
                     " WHERE { ?query sg:queryText ?queryText ;"
                     "                sg:isRelevantTo <" project-id "> . }"))
             (results (query-results->list (system-sparql-query query) #t))]
        (if (null? results)
            '()
            (delete-duplicates
             (apply append
                    (delete '()
                            (map graphs-from-query (apply append results))))
             string=))))
    (lambda (key . args)
      '())))
