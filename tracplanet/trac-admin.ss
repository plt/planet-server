#lang scheme/base

;; the file for administrating trac; for a list of full commands, see
;http://trac.edgewall.org/wiki/TracAdmin
;TAKE CARE: CALLING MOST OF THESE FUNCTIONS LEAVES THE RETURN VALUE, A PORT ;(STDIN) OPEN; CLOSE IT WHEN FINISHED!!

;; /local/pythont/bin/trac-admin /local/bugs/tracfiles/

(require scheme/system
         scheme/string
         scheme/contract
	 scheme/port
         net/url
	 "../configuration.ss"
         "xmlrpc/xml-rpc.ss")
(require (for-syntax scheme/base))


(provide/contract 
 ;[send-command (-> (listof string?) port?)]
 [tickets-get-wrapper (-> (or/c string? false/c) (or/c string? false/c) (listof ticket?))]
 [ticket-get-wrapper (-> integer? ticket?)]
 [ticket-remove (-> (or/c string?  number?) void)]
 [add-component (-> string? string? void?)]
 [list-component (-> (listof string?))]
 [remove-component (-> string? string? null?)]
 [permission-add (-> string? string? null?)]
 [permission-remove (-> string? string? null?)]
 [permission-list (-> (listof string?))]
 [user-exists? (-> string? boolean?)])

;send-command:listofstr->PORT
;NOTE:THIS RETURNS A LIVE (INPUT PORT) PROCESS-OUTPUT PORT. CLOSE IT!
(define (send-command type)
  (let ([returns (apply make-ports (apply process* (list* TRAC-ADMIN TRAC-PATH type)))])
    ((ports-proc returns) 'wait)
    (close-output-port (ports-process-input returns))
    (close-input-port (ports-process-error returns))
    (ports-process-output returns)))
; ticket-get-wrapper : int -> ticket
(define (ticket-get-wrapper tickid)
  (let* ([url (string->url (format TRAC-LOCAL-TICKET-URL tickid))]
         [page (get-pure-port url)]
	 [toc-line (read-line page)])  ;; flush out the table of contents line
    
    ;; (printf "toc-line ~s\n" toc-line)

    (let ([line (get-one-line-of-table page)])
      (close-input-port page)
      (if (equal? (length line)
                  (procedure-arity make-ticket))
          (apply make-ticket line)
          (error 'ticket-get-wrapper 
                 "ticket-get-wrapper, expected ~a elements, got ~a; line:\n ~s"
                 (procedure-arity make-ticket)
		 (length line)
                 line)))))

(define (tickets-get-wrapper owner-filter component-filter)
  (let ([url (string->url (tickets-get-wrapper-url owner-filter component-filter))])
    (call/input-url
     url
     get-pure-port
     (lambda (page)
       (let ([toc-line (read-line page)])  ;; flush out the table of contents line
    
	 ;; (printf "url ~s toc-line ~s\n" (url->string url) toc-line)

	 (let loop ()
	   (let ([raw-line (get-one-line-of-table page)])
	     (cond
	      [(null? raw-line)
	       '()]
	      [else 
	       (let ([line (patch-line raw-line owner-filter component-filter)])
		 (if (equal? (length line)
			     (procedure-arity make-ticket))
		     (cons (apply make-ticket line)
			   (loop))
		     (begin
		       (fprintf (current-error-port)
				"tickets-get-wrapper: expected ~a elements, got ~a; line:\n ~s"
				(procedure-arity make-ticket)
				(length line)
				line)
		       '())))]))))))))

(define (patch-line line owner-filter component-filter)
  (let loop ([line line]
	     [fields ticket-fields])
    (cond
     [(null? fields) '()]
     [(null? line) '()]
     [else
      (let ([field-ent (car fields)])
	(cond
	 [(and owner-filter (eq? field-ent 'owner))
	  (cons owner-filter (loop line (cdr fields)))]
	 [(and component-filter (eq? field-ent 'component))
	  (cons component-filter (loop line (cdr fields)))]
	 [else
	  (cons (car line) (loop (cdr line) (cdr fields)))]))])))

(define (tickets-get-wrapper-url owner-filter component-filter)
  (apply
   string-append
   TRAC-LOCAL-TICKETS-URL
   (map (lambda (x) 
	  (cond
	   [(and (eq? x 'owner) owner-filter)
	    (format "&owner=~a" owner-filter)]
	   [(and (eq? x 'component) component-filter)
	    (format "&component=~a" component-filter)]
	   [else
	    (format "&col=~a" x)]))
	ticket-fields)))

;;ticket-remove:str or num->port
;;removes the given ticket from the repository
(define (ticket-remove tnum)
  (let ([x (if (string? tnum)
               tnum
               (number->string tnum))])
    (close-input-port (send-command (list "ticket remove" x)))))


;;add-component:str str->port
(define (add-component name owner)
  (let ([command (list "component add" 
		       (build-component-name name owner)
		       owner)])
    (close-input-port (send-command command))))

;;permission-add: str str ->null
(define (permission-add group/user permission)
  (close-input-port (send-command (list "permission add" group/user permission))))

;;permission-remove: str str -> null
(define (permission-remove group/user permission)
  (close-input-port (send-command (list "permission remove" group/user permission))))

;void->listof string?
(define (permission-list)
 (port-wrapper (send-command (list "permission list"))))

;;list-component:-->port
(define (list-component)
  (port-wrapper (send-command (list "component list"))))

;;remove-component:str->null
(define (remove-component name owner)
  (let ([command (list "component remove " (build-component-name name owner))])
    (port-wrapper (send-command command))))

(define (build-component-name name owner)
  (format "~a/~a" owner name))

(define (user-exists? username)
  (let* ([passfile (open-input-file TRAC-PASSWORDS)]
         [exists? (regexp-match (regexp (string-append (regexp-quote username) ":")) passfile)])
    (if (boolean? exists?)
        #f
        #t)))



(define (user-add-to-group username groupname)
  (permission-add username groupname))


(define (user-add username password)
  (with-lock (lambda (x y)
               (user-add-unwrapped x y)) username password fail))

    
(define (user-change-password user password)
  (with-lock (lambda (x y)
               (begin (user-remove-unwrapped x)
                      (user-add-unwrapped x y)))
             user password fail))

(define (user-remove user)
  (with-lock (lambda(x y)
               (user-remove-unwrapped x)) user "" fail))

;adds a user to trac's authentication system
;returns string written to permission file
(define (user-add-unwrapped username password)
  (let* ([pcall (list PYTHON USERS.PY "-u" username "-p" password)]
         [process (apply make-ports (apply process* pcall))]
         [line (read-line (ports-process-output process))]
	 [err-string-port (open-output-string)]
         [stderr-thd
	  (thread
	   (lambda () 
	     (copy-port (ports-process-error process) err-string-port)))])
    ((ports-proc process) 'wait)
    (thread-wait stderr-thd)
    (close-input-port (ports-process-output process))
    (close-output-port (ports-process-input process))
    (close-input-port (ports-process-error process))
    (cond
     [(eof-object? line)
      (error 'user-add-unwrapped "python script failed: ~s => ~s" 
	     pcall
	     (get-output-string err-string-port))]
     [else
      (call-with-output-file TRAC-PASSWORDS
	(lambda (passfile)
	  (fprintf passfile "~a\n" line))
	#:exists 'append)])
    (if (eof-object? line)
	(void)
	line)))

(define (user-remove-unwrapped user)
  (let* ([old-p-file       (open-input-file TRAC-PASSWORDS)]
         [temporary (open-output-file TRAC-PASSWORDS-TMP #:exists'replace)]
         [reg (regexp (regexp-quote user))])
    (let loop ()
      (let ([line (read-line old-p-file)])
        (if (eof-object? line)
            (begin0
              (close-output-port temporary)
              (close-input-port old-p-file)
              (system* "/bin/mv" TRAC-PASSWORDS-TMP TRAC-PASSWORDS))
            (if (boolean? (regexp-match reg line))
                (begin  (write-string (string-append line "\n") temporary)
                        (loop))
                (loop)))))))

;=====================================Helper functions

;; get-one-line-of-table : port -> (listof string?)
;; parses a single line of the output, where
;; fields are separated by tabs, but also support
;; quoted fields (which may then contain newlines and other things)
(define (get-one-line-of-table page)

  ;; the sexp below prints out the contents
  ;; of the port to the logfile, for use debugging
  #;
  (let ([p (peeking-input-port page)])
    (let loop ()
      (let ([c (read-char p)])
	(unless (eof-object? c)
		(display c (current-error-port))
		(loop)))))

  (let loop ([in-quotes? #f]
	     [pending-word '()]
	     [prev-char #f])
    (let ([c (read-char page)])
      (if (eof-object? c)
	  (finish pending-word)
	  (case c
	    [(#\newline) 
	     (if in-quotes?
		 (loop #t (cons #\newline pending-word) #\newline)
		 (finish pending-word))]
	    [(#\") 
	     (cond
	      [in-quotes?
	       (let ([next (peek-char page)])
		 (cond
		  [(equal? #\" next)
		   (begin
		     (read-char page)
		     (loop #t (cons #\" pending-word) #\"))]
		  [(member next (list #\newline eof))
		   (list (apply string (reverse pending-word)))]
		  [else
		   ;; must be closing quote, so just skip
		   (loop #f pending-word #\")]))]
	      [(or (equal? prev-char #\tab) (not prev-char))
	       (loop #t '() c)]
	      [else
	       (error 'get-one-line-of-table "found a quote not following a tab")])]
	    [(#\tab) 
	     (if in-quotes?
		 (loop in-quotes? (cons c pending-word) c)
		 (cons (apply string (reverse pending-word))
		       (loop #f '() #\tab)))]
	    [else (loop in-quotes? (cons c pending-word) c)])))))

(define (finish pending-word) 
  (if (null? pending-word)
      '()
      (list (apply string (reverse pending-word)))))

#;
(begin
 (equal? (get-one-line-of-table (open-input-string "a\tb\tc\nd\te\tf"))
         '("a" "b" "c"))
 (equal? (get-one-line-of-table (open-input-string "\"a\""))
	 '("a"))
 (equal? (get-one-line-of-table (open-input-string "b\t\"a\""))
	 '("b" "a"))
 (equal? (get-one-line-of-table (open-input-string "a\t\"a\nb\"\tc"))
         '("a" "a\nb" "c"))
 (equal? (get-one-line-of-table (open-input-string "a\t\"a\nb\""))
         '("a" "a\nb"))
 (equal? (get-one-line-of-table (open-input-string "a\t\"\""))
	 '("a" ""))
 (equal? (get-one-line-of-table (open-input-string "a\\\tb"))
	 '("a\\" "b")))


;==================Wrapper for modifications to password file
;; with-lock : (string? string?-> void) (-> void) -> void
(define (with-lock t user pass fail)
  (dynamic-wind
   (lambda ()
     (let loop ([i 5])
       (if (zero? i)
           (fail)
           (with-handlers ([exn:fail:filesystem:exists?
                            (lambda (exn)
                              (sleep (case i
                                       [(5 4) 1/10]
                                       [(3 2) 1/5]
                                       [(1) 1/2])) 
                              (loop (- i 1)))])
             (close-output-port (open-output-file TRAC-PASSWORD-LOCKFILE #:exists 'error))))))
   (lambda () (t user pass))
   (lambda () 
     (with-handlers ((exn:fail:filesystem:exists? (lambda (x) (void))))
       (delete-file TRAC-PASSWORD-LOCKFILE)))))
 
;raises an exception if the lockfile persists.
(define (fail)
  (error 'trac-admin.ss "Lockfile persists! Please contact administrator for more information."))


(provide/contract
 [user-add (-> string?
	       string? 
	       (or/c void? string?))]
 [user-remove (-> (and/c string? user-exists?) void)]
 [user-change-password (-> string? string? void)]
 [user-add-to-group (-> string? string? void)])
