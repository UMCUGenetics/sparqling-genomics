;;; Copyright © 2018, 2019  Roel Janssen <roel@gnu.org>
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

(define-module (www components query-history)
  #:use-module (www db projects)
  #:use-module (www db queries)
  #:use-module (www util)

  #:export (query-history-component))

(define (strip-prefix-lines query)
  (if (not (string? query))
      ""
      (call-with-output-string
        (lambda (port)
          (for-each (lambda (line)
                      (unless (or (string-prefix? "prefix" line)
                                  (string-prefix? "PREFIX" line)
                                  (string= line ""))
                        (format port "~a~%" line)))
                    (string-split query #\newline))))))

(define* (query-history-component username hash)
  (let [(queries (queries-by-project hash))]
    `((table (@ (id "query-history-table")
                (class "item-table"))
             (tr (th (@ (style "width: auto;")) "Query")
                 (th "Connection")
                 (th (@ (style "white-space: nowrap;")) "Executed by")
                 (th (@ (style "white-space: nowrap;")) "Duration (in seconds)")
                 (th (@ (style "white-space: nowrap; text-align: right;")
                        (colspan "4"))
                     "Actions "
                     (span (@ (class "table-header-small-text"))
                           "(" (a (@ (href ,(string-append
                                             "/query-history-clear/" hash)))
                                  "Remove unselected") ")")))
             ,(delete #f
	       (map (lambda (query)
                      (let* ((id             (basename (query-id query)))
                             (uri            (query-id query))
                             (name           (query-name query))
                             (execution-time (query-execution-time query))
                             (endpoint       (query-endpoint query))
                             (username       (query-username query))
                             (content        (query-content query))
			     (has-name?      (and (string? name)
						  (not (string= name "")))))
			(if (and (string? id)
                                 (string? uri)
                                 (string? endpoint)
                                 (string? username)
                                 (string? content))
			    `(tr (td
                              (div (@ (id ,id)
                                      (class "query-history-text"))
                                   (div (@ (id ,(string-append "queryname-" id))
                                           (onclick ,(js "showQueryText('" id "', '" uri "')"))
                                           ,@(if has-name?
                                                 '()
                                                 `((style "display: none"))))
                                        (strong ,name))
                                   (div (@ (id ,(string-append "querytext-" id))
                                           ,@(if has-name?
                                                 `((style "display: none"))
                                                 '())
                                           (onclick ,(js "showQueryNameForm('" id "', '" uri "')")))
                                        (pre ,(strip-prefix-lines content)))))
                             (td ,endpoint)
                             (td ,username)
                             (td ,(if execution-time execution-time "Unknown"))
                             (td "")
                             (td (@ (class "button-column left-button-column"))
                                 (form (@ (action ,(string-append "/query/" hash))
                                          (method "post"))
                                       (div (@ (class "mark-box-wrapper"))
                                            (input (@ (type "checkbox")
                                                      (id ,(string-append
                                                            "mark-" id))
                                                      (class "mark-box")
                                                      ,(if (query-marked? query)
                                                           '(checked "checked")
                                                           '(name "favorite"))
                                                      (onchange ,(string-append
                                                                  "javascript:toggle_marker('"
                                                                  id "', '" (query-id query)
                                                                  "'); return false"))
                                                      (value ,(query-id query))))
                                            (label (@ (for ,(string-append
                                                             "mark-" id))) ""))))
                             (td (@ (class "button-column left-button-column"))
                                 (form (@ (action ,(string-append "/query/" hash))
                                          (method "post"))
                                       (button (@ (type "submit")
                                                  (class "action-btn remove-btn")
                                                  (name "remove")
                                                  (value ,(query-id query)))
                                               ,(icon 'x-white #t))))
                             (td (@ (class "button-column left-button-column"))
                                 (form (@ (action ,(string-append "/query/" hash))
                                          (method "post"))
                                       (input (@ (type "hidden")
                                                 (name "endpoint")
                                                 (value ,(query-endpoint query))))
                                       (button (@ (type "submit")
                                                  (class "action-btn insert-btn")
                                                  (name "query")
                                                  (value ,(query-content query)))
                                               ,(icon 'up-white #t)))))
 			   #f)))
                    queries)))
      (script (@ (src "/static/js/query-history.js")) ""))))
