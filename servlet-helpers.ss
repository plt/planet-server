#lang scheme
(require (lib "xml.ss" "xml")
         web-server/servlet)

(require "cookie-monster.ss")

(provide send/suspend/nocache send/suspend/doctype)
(define (send/suspend/nocache response)
  (send/suspend
   (λ (k)
     (let ([xexpr (response k)])
       (make-response/full 200 
                           "Okay"
                           (current-seconds)
                           #"text/html" 
                           (list
                            (make-header #"Cache-Control" #"no-cache")
                            (make-header #"Pragma" #"no-cache"))
                           (list doctype (xexpr->string xexpr)))))))

(define (send/suspend/doctype response)
  (send/suspend
   (λ (k)
     (let ([xexpr (response k)])
       (make-response/full 200 
                           "Okay"
                           (current-seconds)
                           #"text/html" 
                           '()
                           (list doctype (xexpr->string xexpr)))))))