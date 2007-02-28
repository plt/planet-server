(module mantis mzscheme
  (require "data-structures.ss" "configuration.ss")
  (require (lib "contract.ss"))
  (require (lib "process.ss"))
  (require (lib "date.ss"))
  (require (prefix srfi13: (lib "13.ss" "srfi")))
  
  (provide/contract
   [create-account (user? string? . -> . any)]
   [associate-user-with-package (user? package? . -> . natural-number/c)])
  (provide exn:fail:mantis?)
   
  
  ;; The mantis interaction functions go through a big kludge: all of mantis's
  ;; internal user creation, project update, etc functions are written in PHP
  ;; and I'd rather call them than port them to Scheme for better robustness.
  ;; This raises the question, though, of how to actually call PHP from Scheme
  ;; code. The solution I came up with was to write a PHP wrapper (pointed to
  ;; by MANTIS-INTERFACE-COMMAND-LINE) that gets run as a unix process whenever
  ;; Scheme wants to call in to PHP. The data to send are always strings that do
  ;; not contain newlines, so they're sent one at a time over standard input. 
  ;; Why not pass them as command-line arguments? Because unprivileged users on
  ;; the machine that hosts planet would be able to see command-line arguments,
  ;; but can't see the contents of the input port.
  ;;
  ;; Anyway, this system is pretty nasty, but hopefully it'll hold up for the time
  ;; being. Probably the more robust way is to expose the functionality I want
  ;; as some kind of SOAP/XMLRPC/web-services something-or-other; if it becomes
  ;; important I'll do that.
  
  ;; create-account : user string -> string
  ;; creates a bug-tracking account (with reporter access) for the given user with
  ;; the given password and returns an initial login cookie.
  ;; 
  ;; No validation is done; it is assumed that the caller has already determined
  ;; the given values to be acceptable.
  (define (create-account user password)
    (run-php-fn "create_mantis_user"
                (user-username user)
                (user-realname user)
                (user-email user)
                password))
  
  ;; associate-user-with-package : user package -> natural-number
  ;; creates a bug database for the given package, makes the given user a developer,
  ;; and returns the mantis DB id for the newly-created mantis project
  (define (associate-user-with-package user package)
    (let ([result-string
           (run-php-fn "create_project_with_user"
                       (format "~a/~a" 
                               (user-username user)
                               (package-name package))
                       (user-username user))])
      (or 
       (string->number result-string)
       (mantis-error
        (format "For some reason create_project_with_user returned non-number result ~s"
                result-string)))))
  
  (define-struct (exn:fail:mantis exn:fail) ())
  
  (define (mantis-error str)
    (raise (make-exn:fail:mantis 
            (string->immutable-string str)
            (current-continuation-marks))))
     
  (define (run-php-fn fname . args)
    (let* ([input-string 
            (srfi13:string-join
             (cons fname args) "\n" 'suffix)]
           [ip (open-input-string input-string)]
           [op (open-output-string)]
           [ep (open-output-string)])
      (parameterize ([current-input-port ip]
                     [current-output-port op]
                     [current-error-port ep])
        (system (MANTIS-INTERFACE-COMMAND-LINE)))
      (let ([output-string (srfi13:string-trim-both (get-output-string op))]
            [error-string (srfi13:string-trim-both (get-output-string ep))])
        (unless (string=? error-string "")
          (mantis-error error-string))
        output-string)))
  
  
  ;; ============================================================
  ;; the above implements some rather low-frequency stuff. But
  ;; querying bugs happens all the time, so for that I bite the
  ;; bullet and write some low-level database-querying code.
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 1 2)))
  (require (lib "class.ss"))
  
  (define-struct bug (summary       ; string
                      submitted-on  ; nat[seconds]
                      url           ; string[url to the view page for this bug]
                      )
    (make-inspector))
  
  (provide (struct bug (summary submitted-on url)))
  (provide/contract
   [get-open-bugs (package? . -> . (union (listof bug?) false/c))])
  
  
  (define (get-open-bugs pkg)
    (cond
      [(not (package-bugtrack-id pkg)) #f]
      [else
       (let* ([query (string-append
                      "SELECT id, summary, "
                      "EXTRACT(SECOND FROM date_submitted) AS sec, "
                      "EXTRACT(MINUTE FROM date_submitted) AS min, "
                      "EXTRACT(HOUR FROM date_submitted) AS hour, "
                      "EXTRACT(DAY FROM date_submitted) AS day, "
                      "EXTRACT(MONTH FROM date_submitted) AS month, "
                      "EXTRACT(YEAR FROM date_submitted) AS year "
                      "FROM mantis_bug_table WHERE "
                      " project_id = "(number->string (package-bugtrack-id pkg)) " AND "
                      " status < 80 "
                      ;; there should also be something to select only public bugs
                      " ORDER BY date_submitted DESC;")]
              [db (connect "localhost" 5432 "bugtracker" "bt" "tracker")])
         (begin0
           (send db map query 
                 (λ (id s . date-parts) 
                   (make-bug 
                    s 
                    (seconds->date
                     (apply find-seconds 
                            (map (λ (x) (or (string->number x) 0))
                                 date-parts)))
                    
                    ((MANTIS-BUG-VIEW-URL) id))))
           (send db disconnect)))])))
