#lang scheme/base
;; the file for administrating trac; for a list of full commands, see
;http://trac.edgewall.org/wiki/TracAdmin
;TAKE CARE: CALLING MOST OF THESE FUNCTIONS LEAVES THE RETURN VALUE, A PORT ;(STDIN) OPEN; CLOSE IT WHEN FINISHED!!

(require scheme/system
         scheme/string
         scheme/contract
         scheme/runtime-path
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
 [ticket-get (-> integer? (listof string?))]
 [ticket-get-wrapper (-> integer? ticket?)]
 [ticket-remove (-> (or/c string?  number?) void)]
 [add-component (-> string? string? void?)]
 [list-component (-> (listof string?))]
 [remove-component (-> string? null?)]
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



;int -> listof string
(define (ticket-get tickid)
  (let* ([pre (pre-table-write (string-append "id=" (number->string tickid)))])
    (if (null? pre)
        pre
        (car (cdr (car pre))))))

;int -> ticket
(define (ticket-get-wrapper tickid)
  (apply make-ticket (ticket-get tickid)))

;;ticket-remove:str or num->port
;;removes the given ticket from the repository
(define (ticket-remove tnum)
  (let ([x (if (string? tnum)
               tnum
               (number->string tnum))])
    (close-input-port (send-command (list "ticket remove" x)))))


;;add-component:str str->port
(define (add-component name owner)
  (let ([command (list "component add"  name  owner)])
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
(define (remove-component comp)
  (let ([command (list "component remove " comp)])
    (port-wrapper (send-command command))))


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
  (let* ([passfile (open-output-file "/local/password/users.txt" #:exists'append )]
         [command (string-append (path->string (build-path here "users.py"))
                                 " -u " 
                                 username
                                 " -p "
                                 password)]
         [process  
          (apply make-ports (process (string-append "/local/pythont/bin/python " command)))]
         [wait ((ports-proc process) 'wait)]
         [line (read-line (ports-process-output process))])
    (begin
      (when (not (eof-object? line))
        (write-string line passfile)
        (write-string "\n" passfile)
        (flush-output passfile)
        (close-output-port passfile)
        (close-input-port (ports-process-output process))
        (close-output-port (ports-process-input process))
        (close-input-port (ports-process-error process))
        line))))

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

;get-table (-> string? (listof (listof string?)))
(define (get-table url)
  (let ([page (get-pure-port (string->url  url))])
    (begin0 
      (let loop () 
        (let ([line (read-line page)])
          (if (eof-object? line)
              '()
              (cons (regexp-split "\t" line) (loop)))))
      (close-input-port page))))

;performs a query, goes to each matching ticket's page and collects the ticket info
;string? -> listof (listof string?)
(define (pre-table-write querystring)
  (let* ([query (ticket-query querystring)])
    (if (null? query)
        '()
        (map (lambda (x)
               (get-table 
                (string-append 
                 (string-append "http://localhost:8080/trac/ticket/"
                                (number->string x))
                 "?format=tab")))
             query))))


    

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
 [user-add (-> (and/c string?  (not/c user-exists?)) string? string?)]
 [user-remove (-> (and/c string? user-exists?) void)]
 [user-change-password (-> (and/c string? user-exists?) string? void)]
 [user-add-to-group (-> string? string? void)])
