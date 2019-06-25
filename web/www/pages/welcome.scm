;;; Copyright © 2017, 2018  Roel Janssen <roel@gnu.org>
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

(define-module (www pages welcome)
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (www util)
  #:use-module (www db connections)
  #:use-module (www db overview)
  #:use-module (sparql driver)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (page-welcome))

(define* (page-welcome request-path username #:key (post-data ""))
  (let ((number-of-endpoints (length (all-connections username))))
    (page-root-template username "Overview" request-path
     `((h2 "Overview"
           (div (@ (class "small-action action-btn-clear-cache"))
                (a (@ (href "/clear-overview-cache")) "↻")))
       (p "There " ,(if (= number-of-endpoints 1) "is " "are ")
          ,number-of-endpoints " configured endpoint"
          ,(if (= number-of-endpoints 1) "" "s") ", which contain"
          ,(if (= number-of-endpoints 1) "s" "") ":")
       (div (@ (class "history-data-loader"))
            (div (@ (class "title")) "Loading overview ...")
            (div (@ (class "content")) "Please wait for the results to appear."))
       (script "
$(document).ready(function(){
  $.get('/overview-table', function(data){
    $('.history-data-loader').replaceWith(data);
  });
});"))
     #:dependencies '(jquery))))
