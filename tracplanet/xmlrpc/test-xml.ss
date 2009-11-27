#lang scheme


(require 
 "xml-rpc.ss"
 "../trac-admin.ss"
 (lib "scheme/system.ss"))             
                       
(define (test-create-ticket)
  (let* ([id (ticket-create-stdin "scheme_test_summaryλ"
                             "scheme_test_descriptionλ"
                             (list (cons "component" "scheme_test_componentλ")
                                   (cons "owner" "scheme_test_ownerλ")))]
        [num_num (number? id)]
        [lamdba? (string=? (ticket-component (ticket-get-wrapper id)) "scheme_test_componentλ")])
    (begin
      (ticket-remove id)
    (and num_num lamdba?))))
      
(define (test-method-help)
  (let* ([port (method-help "ticket.get")]
         [help (not (boolean? (regexp-match "array" (first port))))])
    help))

(define (test-method-list)
  (let* ([string (first (method-list))]
         [listed? (not (boolean? (regexp-match "wiki" string)))])
    listed?))

(define (test-ticket-query)
  (let* ([id (number->string (ticket-create-stdin "hi" "hi" '()))]
         [result1 (ticket-query (string-append "id=" id))])
    (begin
      (ticket-remove id)
      (not (empty? result1)))))

         
;; broken test?
#;
(define (test-ticket-get)
  (let* ([id  (ticket-create-stdin "remove" "remove" '())]
         [both?   (and 
                   (empty? (ticket-get 1000000000))
                   (cons? (ticket-get id)))])
    (begin 
      (ticket-remove id)
      both?)))
   
(define (test-all)
  (foldr (lambda(x y) (and x y)) #t  (list 
                                      (test-create-ticket)
                                      (test-method-help)
                                      (test-method-list)
                                      (test-ticket-query)
                                      ;;(test-ticket-get)  ;; broken test?
                                )))