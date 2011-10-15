#lang scheme/base

(provide spawn-bug-closer-thread
	 compute-top-bug-closers
         top-bug-closers)
(require  "tracplanet/trac-admin.ss"
          "tracplanet/xmlrpc/xml-rpc.ss")

(define delay (* 60 60)) ;; one hour
(define results-file "/home/wwwplanet/planet/top-bug-closers")
;(define results-file "/tmp/top-bug-closers")

(define (spawn-bug-closer-thread)
  (thread
   (lambda ()
     (sleep 2) ;; wait a little before the first time it is recomputed after a restart
     (let loop ()
       (compute-and-write-top-bug-closers)
       (sleep delay)
       (loop)))))

(define (compute-and-write-top-bug-closers)
  (let ([now (struct->vector (seconds->date (current-seconds)))])
    (let-values ([(vals cpu real gc) 
		  (time-apply (lambda () 
				(with-handlers ([exn:fail? exn->message])
					       (compute-top-bug-closers)))
			      '())])
      (with-lock
       (lambda ()
         (call-with-output-file results-file
           (lambda (port)
             (write now port)
             (newline port)
             (write (car vals) port)
             (newline port)
             (write (list 'time-to-compute 'cpu cpu 'real real 'gc gc) port)
             (newline port))
           #:exists 'truncate))))))

(define (exn->message exn)
  (let ([sp (open-output-string)])
    (parameterize ([current-error-port sp])
		  ((error-display-handler) (exn-message exn) exn))
    (get-output-string sp)))

;; top-bug-closers : -> (values (or/c #f number) (or/c string? (listof (list number string))))
(define (top-bug-closers)
  (with-handlers ((exn:fail:filesystem? (lambda (x) (values #f "")))
                  (exn:fail:read? (lambda (x) (values #f ""))))
    (with-lock
     (lambda ()
       (call-with-input-file results-file
         (lambda (port)
           (values (vec->date (read port))
		   (read port))))))))

(define (vec->date v) (apply make-date (cdr (vector->list v))))

(define (compute-top-bug-closers)
  (let* ([query-results (ticket-query "status=closed&max=1000")]
         [tickets (map ticket-get-wrapper query-results)]
         [hash-table (make-hash)])
    (for-each (lambda (ticket)
		(let ([closer (ticket-owner ticket)])
		  (unless 
		   (equal? closer "robby")
		   (unless
		    (equal? (ticket-reporter ticket) closer)
		    (let* ([value (hash-ref hash-table closer 0)])
		      (hash-set! hash-table closer (+ 1 value)))))))
              tickets)
    (let* ([hash-list (hash-map hash-table (lambda (x y) (list y x)))]
           [sorted (sort hash-list compare-rows)]
	   [min-count (min (length sorted) 3)]
	   [top3 (take-it sorted min-count)]
	   [rest (drop-it sorted min-count)])
      (if (null? top3)
	  top3
	  (let ([lowest (list-ref (last top3) 0)])
	    (append
	     top3
	     (let loop ([others rest])
	       (cond
		[(null? others) null]
		[else (let ([fst (car others)])
			(if (= lowest (list-ref fst 0))
			    (cons fst (loop (cdr others)))
			    '()))]))))))))

(define (compare-rows x y)
  (cond
    [(= (car x) (car y))
     (string-ci<=? (cadr x) (cadr y))]
    [else
     (> (car x) (car y))]))

(define (last lst)
  (cond
   [(null? (cdr lst)) (car lst)]
   [else (last (cdr lst))]))

; list? exact-nonneg-int? -> (and/c list? (=? (length list) exact-nonneg int))
(define (take-it lista number)
  (if (or (zero? number) (null? lista))
      '()
      (cons (car lista) (take-it (cdr lista) (- number 1)))))

(define (drop-it lista number)
  (if (or (zero? number) (null? lista))
      lista
      (drop-it (cdr lista) (- number 1))))

(define with-lock
  (let ([lock (make-semaphore 1)])
    (lambda (f)
      (dynamic-wind
       (lambda () (semaphore-wait lock))
       f
       (lambda () (semaphore-post lock))))))
