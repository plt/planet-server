#lang scheme
(require web-server/servlet/bindings
         web-server/private/request-structs
         "servlet-helpers.ss")

(provide (struct-out demand-page))
(define-struct demand-page (>problems->k->html >demands))

(provide/contract
 [send/suspend/demand
  (demand-page? . -> . request?)])

(define (send/suspend/demand demand-page)
  (let ([problems->k->html (demand-page->problems->k->html demand-page)]
        [demands (demand-page->demands demand-page)])
    (let loop ([problems '()])
      (let ([req (send/suspend/doctype (problems->k->html problems))])
        (let ([new-problems (demands (request-bindings req))])
          (if (null? new-problems)
              req
              (loop new-problems)))))))