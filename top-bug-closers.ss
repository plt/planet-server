#lang scheme/base

(provide spawn-bug-closer-thread
         top-bug-closers)
(require  "tracplanet/trac-admin.ss"
          "tracplanet/xmlrpc/xml-rpc.ss")

(define delay (* 60 60)) ;; one hour
(define results-file "/local/planet/top-bug-closers")

(define (spawn-bug-closer-thread)
  (thread
   (lambda ()
     (sleep 2) ;; wait a little before the first time it is recomputed after a restart
     (let loop ()
       (let ([now (struct->vector (seconds->date (current-seconds)))]
             [top (compute-top-bug-closers)])
         (with-lock
          (lambda ()
            (call-with-output-file results-file
              (lambda (port)
                (write now port)
                (display "\n" port)
                (write top port)
                (display "\n" port))
              #:exists 'truncate))))
       (sleep delay)
       (loop)))))

;; top-bug-closers : -> (values (or/c #f number) (listof (list number string)))
(define (top-bug-closers)
  (with-handlers ((exn:fail:filesystem? (lambda (x) (values #f '())))
                  (exn:fail:read? (lambda (x) (values #f '()))))
    (with-lock
     (lambda ()
       (call-with-input-file results-file
         (lambda (port)
           (values (vec->date (read port))
                   (read port))))))))

(define (vec->date v) (apply make-date (cdr (vector->list v))))

(define (compute-top-bug-closers)
  (let* ([query-results (ticket-query "status=closed")]
         [bug-closers (map ticket-owner (map ticket-get-wrapper query-results))]
         [hash-table (make-hash)])
    (for-each (lambda(x)
                (let* ([value (hash-ref hash-table x 0)])
                  (hash-set! hash-table x (+ 1 value))))
              bug-closers)
    (let* ([hash-list (hash-map hash-table (lambda (x y) (list y x)))]
           [sorted (sort hash-list compare-rows)])
      (take-it sorted (min (length sorted) 3)))))

(define (compare-rows x y)
  (cond
    [(= (car x) (car y))
     (string-ci<=? (cadr x) (cadr y))]
    [else
     (> (car x) (car y))]))

; list? exact-nonneg-int? -> (and/c list? (=? (length list) exact-nonneg int))
(define (take-it lista number)
  (if (or (zero? number) (null? lista))
      '()
      (cons (car lista) (take-it (cdr lista) (- number 1)))))


(define with-lock
  (let ([lock (make-semaphore 1)])
    (lambda (f)
      (dynamic-wind
       (lambda () (semaphore-wait lock))
       f
       (lambda () (semaphore-post lock))))))

(compute-top-bug-closers)