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

(define* (page-welcome request-path #:key (post-data ""))
  (let ((number-of-endpoints (length (all-connections))))
    (page-root-template "Overview" request-path
     `((h2 "Overview")
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
