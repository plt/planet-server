#lang scheme/base

;; the file for administrating trac; for a list of full commands, see
;http://trac.edgewall.org/wiki/TracAdmin
;TAKE CARE: CALLING MOST OF THESE FUNCTIONS LEAVES THE RETURN VALUE, A PORT ;(STDIN) OPEN; CLOSE IT WHEN FINISHED!!

;; /local/pythont/bin/trac-admin /local/bugs/tracfiles/

(require scheme/system
         scheme/string
         scheme/contract
         scheme/runtime-path
	 scheme/port
         net/url
         "xmlrpc/xml-rpc.ss")
(require (for-syntax scheme/base))


(define tracpath "/local/bugs/tracfiles")
(define templatepath "/usr/share/trac/templates")
(define repospath "/local/bugs/tracrepos")
(define dbstring "sqlite:db/trac.db")
;(define here "/local/svn/iplt/planet/tracplanet")
(define-runtime-path here ".")
;when we know where the stuff is, switch to postgres
;(define dbstring "postgres://johndoe:letmein@localhost/trac")



(provide/contract 
 ;[send-command (-> (listof string?) port?)]
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
;send a trac-admin command to trac; see Trac wiki for more info on trac commands
;NOTE:THIS RETURNS A LIVE (INPUT PORT) PROCESS-OUTPUT PORT. CLOSE IT!
(define (send-command type)
  (let ([returns (apply make-ports 
                        (apply process* (append (list 
                                                 "/local/pythont/bin/trac-admin" 
                                                 tracpath)
                                                type)))])
    ;wait until the process has finished before returning to thread; avoids a race-condition
    ((ports-proc returns) 'wait)
    (close-output-port (ports-process-input returns))
    (close-input-port (ports-process-error returns))
    (ports-process-output returns)))

; ticket-get-wrapper : int -> ticket
(define (ticket-get-wrapper tickid)
  (let* ([url (string->url (format "http://localhost:8080/trac/ticket/~a?format=tab" tickid))]
         [page (get-pure-port url)])
    (read-line page) ;; flush out the table of contents line
    (let ([line (get-one-line-of-table page)])
      (close-input-port page)
      (if (equal? (length line)
                  (procedure-arity make-ticket))
          (apply make-ticket line)
          (error 'ticket-get-wrapper 
                 "parsed line ~s, expected ~a elements"
                 line
                 (procedure-arity make-ticket))))))

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
  (let* ([passfile (open-input-file "/local/password/users.txt")]
         [exists? (regexp-match (regexp (string-append username ":")) passfile)])
    (if (boolean? exists?)
        #f
        #t)))



(define (user-add-to-group username groupname)
  (permission-add username groupname))


(define (user-add username password)
  (with-lock (lambda (x y)
               (user-add-unwrapped x y)) username password fail))

    
(define (user-change-password user password)
  (with-lock (lambda(x y)
               (and (user-remove-unwrapped x)
                    (user-add-unwrapped x y))) user password fail))

(define (user-remove user)
  (with-lock (lambda(x y)
               (user-remove-unwrapped x)) user "" fail))

;adds a user to trac's authentication system
;returns string written to permission file
(define (user-add-unwrapped username password)
  (let* ([process  
          (apply make-ports (process* "/local/pythont/bin/python"
				      "/local/svn/iplt/planet/tracplanet/users.py"
				      "-u"
				      username
				      "-p"
				      password))]
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
      (error 'user-add-unwrapped "python script failed: ~s" (get-output-string err-string-port))]
     [else
      (call-with-output-file  "/local/password/users.txt"
	(lambda (passfile)
	  (fprintf passfile "~a\n" line))
	#:exists 'append)])
    (if (eof-object? line)
	(void)
	line)))

(define (user-remove-unwrapped user)
  (let* ([old-p-file       (open-input-file "/local/password/users.txt" )]
         [temporary (open-output-file "/local/password/temp.tmp" #:exists'replace)])
    (let loop ()
      (let ([line (read-line old-p-file)])
        (if (eof-object? line)
            (begin0
              (close-output-port temporary)
              (close-input-port old-p-file)
              (system* "/bin/mv" "/local/password/temp.tmp" "/local/password/users.txt"))
            (if (boolean? (regexp-match (regexp user) line))
                (and (write-string (string-append line "\n") temporary)
                     (loop))
                (loop)))))))

;=====================================Helper functions

;; get-one-line-of-table : port -> (listof string?)
;; parses a single line of the output, where
;; fields are separated by tabs, but also support
;; quoted fields (which may then contain newlines and other things)
(define (get-one-line-of-table page)
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
	       ;; must be closing quote, so just skip
	       (loop #f pending-word #\")]
	      [(or (equal? prev-char #\tab) (not prev-char))
	       (loop #t '() c)]
	      [else
	       (error 'get-one-line-of-table "found a quote not following a tab")])]
	    [(#\\)
	     (let ([nc (read-char page)])
	       (if (eof-object? nc)
		   (error 'get-one-line-of-table "found a backslash followed by eof")
		   (loop in-quotes? (cons nc pending-word) #\")))]
	    [(#\tab) (cons (apply string (reverse pending-word))
			   (loop #f '() #\tab))]
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
 (equal? (get-one-line-of-table (open-input-string "a\\\""))
	 '("a\"")))


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
                     (close-output-port (open-output-file "/local/password/lockfile.lock" #:exists 'error))))))
           (lambda() (t user pass))
           (lambda() (with-handlers ((exn:fail:filesystem:exists? (lambda (x) (void))))
             (delete-file "/local/password/lockfile.lock")))))
 
;raises an exception if the lockfile persists.
(define (fail)
  (raise "Lockfile persists! Please contact administrator for more information."))


(provide/contract
 [user-add (-> string?
	       string? 
	       (or/c void? string?))]
 [user-remove (-> (and/c string? user-exists?) void)]
 [user-change-password (-> (and/c string? user-exists?) string? void)]
 [user-add-to-group (-> string? string? void)])
