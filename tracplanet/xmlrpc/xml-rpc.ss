#lang scheme

(require scheme/port
         scheme/system
	 "../../configuration.ss"
         (lib "process.ss")
         (lib "string.ss")
         (lib "list.ss")
         (lib "srfi/13.ss"))

(define url (format "http://~a/trac/xmlrpc" TRAC-HOST))
(define xmlrpc "/home/wwwplanet/git/planet-server/tracplanet/xmlrpc")

(provide/contract 
 [ticket-fields (listof symbol?)]
 [py_script_execute (-> string? (listof string?) boolean? (or/c port? (listof string?)))]
 [a_string (-> (listof (cons/c string? string?)) (listof string?))]
 [port-wrapper (-> port? (listof string?))]
 [ticket-create-stdin (-> string? string? (listof (cons/c string? string?)) number?)]
 [ticket-query (-> string? (listof number?))]
 [strip (-> string? number?)]
 [method-list (-> (listof string?))]
 [method-help (-> string? (listof string?))])


(define-struct ports
  (process-output   ;input-port
   process-input  ;out port
   pid     ;int
   process-error  ;port
   proc)   ;function
  )

(define-syntax (ticket-fields/ds stx)
  (syntax-case stx ()
    [(_ ticket ticket-fields)
     (with-syntax ([fields
		    #'(id
		       summary
		       reporter
		       owner
		       description
		       type
		       status
		       priority
		       milestone
		       component
		       resolution
		       keywords
		       cc
		       planetversion
		       pltversion)])
		  #'(begin
		      (define-struct ticket fields)
		      (define ticket-fields 'fields)))]))

(ticket-fields/ds ticket ticket-fields)

(provide (struct-out ports))
(provide (struct-out ticket))


;string? string? (listof (cons string? string?) -> number?
(define (ticket-create-stdin summary description attributes)
  (let* ([att-string (a_string attributes)])
    (string->number (first (py-stdin-options (string-append xmlrpc "/ticketcreatestdin.py") 
                                             (append att-string (list url summary description)))))))



(define (method-help method)
  (py_script_execute (string-append xmlrpc "/methodHelp.py") (list url method) #t))

(define (method-list)
  (py_script_execute (string-append xmlrpc "/methodlist.py") (list url) #t))

;string? -> (listof numbers?)
;returns a list of numbers matching query string (using the format where "reporter=mr.jones" would be (list (cons "reporter" "mr.jones")))
(define (ticket-query qstring)
  (let* ([pre-list (py_script_execute
                    (string-append xmlrpc "/ticket_query.py")
                    (list url qstring)
                    #t)]
	 [list-t  (if (cons? pre-list) 
		     (string-tokenize
                       (first pre-list))
	             '("[]"))] 
         [empty-query? (equal? list-t '("[]"))])
    (if empty-query?
        '()
        (letrec ([iterator (lambda (ticket-ids list-string)
                         (if (cons? list-string)
                             (iterator (cons (strip (first list-string)) ticket-ids) (rest list-string))
                             ticket-ids))])
      (iterator '() list-t)))))



;=======================Helper Functions=========================================================================
;used to return a list of strings (1/line of output) from port. Closes port.
;port?->listof string?
(define (port-wrapper port)
  (let reader ([lines '()])
    (let* ([line (read-line port)]
           [file-done (eof-object? line)])
      (if (not file-done)
          (reader (cons line lines))
          (begin (close-input-port port)
                 (reverse lines)
                 )))))

;takes a string and removes the last character (and maybe first) character to return a number
;used for cleaning up ticket-numbers from a query
; string -> num
;INVARIANT: string must be of the form "1char_int" or "1char_int_1char"
(define (strip string)
  (let* ([number   (string->number (substring string 0 (- (string-length string) 1)))]
         [strippedf (substring string 1 (- (string-length string) 1))])
    (if (number? number)
        number
        (string->number strippedf))))

;used to format the query string
(define (a_string listof_tuples)
  (foldl (lambda(x sendable_string)
           (append sendable_string (list (string-append "--" (car x) "=" (cdr x) " "))))
         '() listof_tuples))

(define (py_script_execute pyscript listof_args stdinot)
  (let ([returns   (apply make-ports
                          (apply process* 
                                 (cons pyscript listof_args)))])
    ((ports-proc returns) 'wait)
    (close-output-port (ports-process-input returns))
    (close-input-port (ports-process-error returns))
    (if stdinot
        (port-wrapper (ports-process-output returns))
        (ports-process-output returns))))



;used to create tickets with optional arguments
(define (py-stdin-options pyscript listofargs)
  (let* ([returns   (apply make-ports
                           (process* pyscript))])
    (begin
      (print (length listofargs) (ports-process-input returns))
      (write-string "\n" (ports-process-input returns))
      (flush-output (ports-process-input returns))
      (map (lambda(x)
             (let* ([bytes (string->bytes/utf-8 x)]
                    [length (bytes-length bytes)])
               (begin 
                 (print (+ 1 length) (ports-process-input returns))
                 (write-string "\n" (ports-process-input returns))
                 (write-bytes (string->bytes/utf-8 x) (ports-process-input returns))
                 (write-string "\n" (ports-process-input returns))
                 (flush-output (ports-process-input returns)))))
           listofargs)    
      
      ((ports-proc returns) 'wait)
      (port-wrapper (ports-process-output returns)))))
