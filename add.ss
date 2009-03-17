#lang scheme
(require "db.ss" "package-creation.ss" "data-structures.ss" "html.ss" "configuration.ss" "demands.ss" "demands-servlet.ss"
         "user-utilities.ss" "cookie-monster.ss" "servlet-helpers.ss")
(require "tracplanet/trac-admin.ss")
(require xml/xml
         web-server/servlet
         scheme/path
         scheme/list
         web-server/servlet/bindings
         web-server/managers/lru
         net/url
         net/sendmail
         scheme/date
         (prefix-in srfi1: srfi/1)
         (prefix-in srfi13: srfi/13))

(provide interface-version start manager)
(define interface-version 'v2)
(define memory-threshold (* 768 1024 1024))
(define manager 
  #;(make-threshold-LRU-manager #f memory-threshold)
  (create-LRU-manager
   ;; Called when an instance has expired.
   #f
   ;; The condition below is checked every 5 seconds
   5
   ;; One 'life point' is deducted every 10 minutes
   (* 10 60)
   ;; If this condition is true a 'life point' is deducted
   ;; from the continuation
   (lambda ()
     (define memory-use (current-memory-use))
     (define collect?   (or (>= memory-use memory-threshold)
                            (< memory-use 0)))
     collect?)
   ;; The number of 'life points' an continuation starts with
   #:initial-count 24
   ;; Logging done whenever an continuation is collected
   #:inform-p (lambda args
                (void))))

(startup)

(define (extract key items)
  (srfi1:filter-map (lambda (pr) (and (eq? (car pr) key) (cadr pr))) items))

(define (strings->error-xhtml strs)
  (cond
    [(null? strs) '()]
    [else
     `((div ((class "errorBox"))
            (ul
             ,@(map (lambda (msg) `(li ,msg)) strs))))]))

(define (get request symbol)
  (extract-binding/single symbol (request-bindings request)))
(define (get-all request symbol)
  (extract-bindings symbol (request-bindings request)))
(define (get* request symbol process)
  (let ([binding 
         (with-handlers ([exn:fail? (λ (_) #f)])
           (get request symbol))])
    (if binding
        (process binding)
        #f)))

(define (get-filename request symbol)
  (let* ([raw (request-bindings/raw request)]
         [binding (get-first (lambda (b)
                               (and (binding:file? b)
                                    (bytes=? (binding-id b) 
                                             (string->bytes/utf-8 (symbol->string symbol)))))
                             raw)])
    (unless binding
      (error 'get-filename "No file-binding for ~e exists in the given bindings ~s" symbol raw))
    (binding:file-filename binding)))

;; get-first : (X -> bool) (listof X) -> (union X #f)
(define (get-first p l) (ormap (lambda (x) (and (p x) x)) l))

(define (start initial-request)
  
  (define (login r fail-with)
    (let ([login (get r 'login)]
          [pw    (get r 'pw)])
      (cond
        [(get-user-record login pw)
         => 
         (λ (u)
           (let* ([passcode (log-user-in u)]
                  [req (send/suspend
                        (λ (k) 
                          (build-cookie-forwarder 
                           k
                           (list (build-cookie "username" login #:path "/") 
                                 (build-cookie "passcode" passcode #:path "/")))))])
             (logged-in-actions u req)))]
        [else 
         (fail-with `((password  (message "Incorrect username or password.")) (login (value ,login))))])))
  
  (define (page titles bodies)
    (mkdisplay titles bodies initial-request))
  
  (define (LOGIN-PAGE problems)
    (let* ([general-error-messages (strings->error-xhtml (extract 'general problems))]
           [extractor
            (lambda (field wrapper)
              (lambda (key)
                (let ([pr (assq key problems)])
                  (if pr
                      (let ([pr2 (assq field (cdr pr))])
                        (if pr2
                            (list (wrapper (cdr pr2)))
                            '()))
                      '()))))]
           [value-for (extractor 'value (λ (x) `(value ,@x)))]
           [message-for (extractor 'message (λ (x) `(small ((class "errorMsg")) ,@x)))])
      
      (lambda (k)
        (page
         (list "contribute")
         `((p "Thanks for deciding to contribute a package to PLaneT! "
              "You can either create an account using the form below, or, "
              "if you already have an account, log in directly.")
           ,(section "Log in")
           ,@general-error-messages
           (div 
            (div ((id "logIn") (class "filledin"))
                 (form ((action ,k) (method "post"))
                       (input ((type "hidden") (name "mode") (value "login")))
                       (p (b "Already have a user account? Then log in here."))
                       (table
                        (tr (td "User name")
                            (td (input ((name "login") 
                                        (type "text")
                                        ,@(value-for 'login))) ,@(message-for 'login)))
                        (tr (td "Password")
                            (td (input ((name "pw") (type "password"))) ,@(message-for 'password)))
                        (tr (td ((colspan "2")) (input ((type "submit") (value "Log in")))
                                (br)
                                (small (a ((href ,(string-append k "?mode=resetpass"))) "I forgot my password")))))))
            
            (div ((id "createUser") (class "filledin"))
                 (form ((action ,k) (method "post"))
                       (input ((type "hidden") (name "mode") (value "create")))
                       (p (b "Need a user account? Create one now."))
                       (table
                        (tr (td "Desired username")
                            (td (input ((type "text") (name "username") ,@(value-for 'username))))
                            (td ,@(message-for 'username)))
                        (tr (td "Your real name")
                            (td (input ((type "text") (name "realname") ,@(value-for 'realname)))) 
                            (td ,@(message-for 'realname)))
                        (tr (td "Your email address")
                            (td (input ((type "text") (name "email") ,@(value-for 'email))))
                            (td ,@(message-for 'email)))
                        (tr (td "A password")
                            (td (input ((type "password") (name "password1"))))
                            (td ,@(message-for 'password1)))
                        (tr (td "Password again")
                            (td (input ((type "password") (name "password2")))) 
                            (td ,@(message-for 'password2)))
                        (tr (td ((colspan "2")) (input ((type "submit") (value "Create account"))))
                            (td nbsp))))))
           (div ((style "clear: both;")) nbsp))))))
  
  (define (create r fail-with)
    (let-values ([(problems defaults) (get-problems-and-defaults (request-bindings r))])
      (cond
        [(pair? problems) (fail-with (append problems defaults))]
        [else
         (let ([username (srfi13:string-trim-both (get r 'username))]
               [realname (srfi13:string-trim-both (get r 'realname))]
               [email (srfi13:string-trim-both (get r 'email))]
               [password (get r 'password1)])
           (verify-email-address email)
           (let ([user (create-new-user username realname email password)]
                 [trac-user (when (not (user-exists? username))
                              (user-add username password))])
             
             (main-interaction-loop (car (request->repository r)) user)))])))
  
  (define (reset-password)
    (let loop ([problems '()])
      (let ([r (send/suspend/doctype (RESET-PASSWORD-PAGE problems))])
        (let* ([demands 
                (all-demands
                 (list
                  (fields-nonblank '(username))
                  (fields-ascii 'username)
                  (field-constraint
                   (wrap-as-demand-p 
                    username-taken? 
                    (λ (n) `(username (message "No username " (b ,n) " exists"))))
                   'username)))]
               [new-problems (demands (request-bindings r))])
          (cond
            [(null? new-problems)
             (let ([user (get-user-record/no-password (get r 'username))])
               (begin
                 ; send an email to the address associated with the account, 
                 ; the followup to which resets the password
                 (send/suspend/doctype (PASSWORD-RESET-EMAIL/PAGE user))
                 ; if send-email returns the email recipient got the message
                 ; and is in control, so go ahead and let them change the password
                 (do-passwordless-reset user)))]
            [else (loop new-problems)])))))
  
  (define (RESET-PASSWORD-PAGE problems)
    (with-problems problems
                   (λ (general-errors value-for message-for)
                     (lambda (k)
                       (page
                        '("Reset password")
                        `((section "Reset your password")
                          (p "For security purposes, we cannot send you your password directly. "
                             "However, we can allow you to choose a new password "
                             " as long as you can verify that you control the email address "
                             " associated with your username.")
                          (form ((action ,k) (method "post"))
                                (table (tr (td "Username:") 
                                           (td (input ((type "text") (name "username"))) ,@(message-for 'username)))
                                       (tr (td ((colspan "2")) 
                                               (input ((type "submit") 
                                                       (value "Send a confirmation email")))))))))))))
  
  
  ;; send an email to the given user with a link allowing them to continue, and return a web page indicating this has been done
  (define (PASSWORD-RESET-EMAIL/PAGE user)
    (lambda (k) 
      (when (SEND-EMAILS?)
        (send-mail-message "PLaneT <planet@plt-scheme.org>" 
                           "A password reset request has been made for this account"
                           (list (user-email user))
                           '()
                           '()
                           (list "Greetings! You (or someone claiming to be you) has asked to reset the password for the "
                                 "account " (user-username user) " on PLaneT, the PLT Scheme package repository. "
                                 "Our records indicate that that account belongs to this email address. If you really want to "
                                 "reset your password, then please visit the following URL: "
                                 ""
                                 (url->string (combine-url/relative (EXTERNAL-URL-ROOT) k))
                                 ""
                                 "If you do not want to reset the password to your PLaneT account, then please disregard "
                                 "this message."
                                 ""
                                 "Thanks,"
                                 "PLaneT")))
      (page 
       '("Confirm email")
       `((section "Confirmation message sent")
         ,(if (SEND-EMAILS?)
              `(p "We have sent an email to the address we have listed as belonging to the user " (b ,(user-username user))". "
                  "For security purposes, you must visit the link provided in that message within 48 hours to proceed. If you have further questions, "
                  "please contact a PLaneT administrator by emailing planet@plt-scheme.org for help.")
              `(p "Click " (a ((href ,k)) "here") " to continue."))))))
  
  
  (define (verify-email-address email)
    (send/suspend/doctype
     (lambda (k) 
       (when (SEND-EMAILS?)
         (send-mail-message "PLaneT <planet@plt-scheme.org>" 
                            "Please verify your email address"
                            (list email)
                            '()
                            '()
                            (list "Greetings! You (or someone claiming to be you) have signed up for an account "
                                  "with PLaneT, the PLT Scheme package repository. To verify your email address, "
                                  "please visit the following URL: "
                                  ""
                                  (url->string (combine-url/relative (EXTERNAL-URL-ROOT) k))
                                  ""
                                  "within 48 hours. If you do not want to create an account with PLaneT, then please disregard "
                                  "this message."
                                  ""
                                  "Thanks,"
                                  "PLaneT")))
       (page
        '("Confirm email address")
        `(,(if (SEND-EMAILS?)
               `(p "To complete the email change process, please check the email account "
                   (b ,email) " for a message telling you how to proceed.")
               `(p "Click " (a ((href ,k)) "here") " to continue.")))))))
  
  ;; do-passwordless-reset : user -> void 
  (define (do-passwordless-reset user)
    (let* ([demands
            (all-demands
             (list
              (fields-ascii 'password1 'password2)
              (field-constraint 
               (wrap-as-demand-p string=? (lambda (a b) "Passwords did not match"))
               'password1 'password2)))]
           [PAGE (PASSWORDLESS-RESET-PAGE user)])
      (let loop ([problems '()])
        (let* ([r (send/suspend/doctype (PAGE problems))]
               [problems (demands (request-bindings r))])
          (cond
            [(null? problems)
             ; update the user's password and then send to the logged-in state
             (user-change-password (user-username user) (get r 'password1))
             (update-user-password user (get r 'password1))
             (user-change-password (user-username user) (get r 'password1))
             (main-interaction-loop (car (request->repository r)) user)]
            [else (loop problems)])))))
  
  (define ((PASSWORDLESS-RESET-PAGE user) problems)
    (lambda (k)
      (page
       '("password reset")
       `((p "You have now validated yourself as user " (b ,(user-username user))
            " by responding to email. Please use this form to reset your password.")
         ,(section "Reset your password")
         (form ((action ,k) (method "post"))
               (table
                (tr (td "Username:")     (td (b ,(user-username user))))
                (tr (td "New password:") (td (input ((type "password") (name "password1")))))
                (tr (td "Again:")        (td (input ((type "password") (name "password2")))))
                (tr (td ((colspan "2")) (input ((type "submit") (value "Reset password")))))))))))
  
  (define (logged-in-actions user r)
    (cond
      [(and (exists-binding? 'mode (request-bindings r))
            (string=? (extract-binding/single 'mode (request-bindings r)) "logout"))
       (log-user-out user)
       (logged-out-actions)]
      [else
       (let* ([rep/? (request->repository r)]
              [rep (car rep/?)])
         (main-interaction-loop rep user))]))
  
  (define (logged-out-actions)
    (let loop ([problems '()])
      (let ([r (send/suspend/nocache (LOGIN-PAGE problems))])
        (if (not (exists-binding? 'mode (request-bindings r)))
            (loop '((general "Your browser did not submit the " (b "mode") " binding, which is necessary")))
            (case (string->symbol (get r 'mode))
              [(login) (login r loop)]
              [(create) (create r loop)]
              [(resetpass) (reset-password)] 
              [else (loop '((general "The mode you used doesn't make sense.")))])))))
  
  ;; the actual handler
  (cond 
    [(logged-in-user initial-request) 
     => (λ (u) (logged-in-actions u initial-request))]
    [else (logged-out-actions)]))

;; ============================================================
;; logged-in-user stuff
(define (main-interaction-loop repository user)
  
  (define (page titles bodies) (mkdisplay* titles bodies repository user))
  
  ;; html pages
  (define (main-loop-page problems)
    (with-problems problems
                   (λ (general-errors value-for message-for)
                     (lambda (k)
                       (define packages (user->packages/no-repositories user)) 
                       (define (package->rows pkg)
                         (let ([v (car (package-versions pkg))])
                           `((tr ((class "filledin")) 
                                 (td (a ((href ,(string-append k "?action=fulledit&package=" (number->string (package-id pkg)))))
                                        ,(package-name pkg))) 
                                 (td ,(format "~a.~a"
                                              (pkgversion-maj v)
                                              (pkgversion-min v)))
                                 (td (small "["
                                            (a ((href ,(string-append k "?action=update&package=" 
                                                                      (number->string (package-id pkg)))))
                                               "update this package")
                                            "] or ["
                                            (a ((href ,(string-append k "?action=edit&pkgversion=" 
                                                                      (number->string (pkgversion-id v)))))
                                               "edit package metadata")
                                            "]")))
                             (tr ((class "filledin"))
                                 (td ((colspan "3")) ,@(or (package-blurb pkg) '("[no package description]"))))
                             (tr ((class "filledin"))
                                 (td ((colspan "3")) ,@(or (pkgversion-blurb v)
                                                           `("[no version notes]")))))))
                       (page
                        (list "Your packages")
                        `(,@(if (null? general-errors)
                                '()
                                `((div ,@(map (λ (msg) `(p ,msg)) general-errors))))
                          
                          
                          ,(section "Contribute a package")
                          (form ((action ,k) (method "post") (enctype "multipart/form-data"))
                                (p "Contribute a new package: "
                                   (input ((type "file") (name "file")))
                                   (input ((type "submit") (value "Upload")))
                                   (input ((type "hidden") (name "action") (value "newpackage"))))
                                ,@(message-for 'contribute))
                          ,(section "Manage your packages")
                          ,@(cond
                              [(null? packages) '()]
                              [else
                               `((p "These are your packages:")
                                 (table ((id "yourPackages")) ,@(apply append (map package->rows packages))))])
                          ,(section "Manage your account")
                          (table ((width "95%"))
                                 (tr 
                                  (td ((valign "top"))
                                      (form ((action ,k) (method "post") (id "changePassword"))
                                            (input ((type "hidden") (name "action") (value "setpassword")))
                                            (p (b "Change your password"))
                                            (table
                                             (tr (td "Old password:") 
                                                 (td (input ((type "password") (name "oldpass")))
                                                     ,@(message-for 'oldpass)))
                                             (tr (td "New password:") 
                                                 (td (input ((type "password") (name "newpass1")))
                                                     ,@(message-for 'newpass1)))
                                             (tr (td "Again:") 
                                                 (td (input ((type "password") (name "newpass2")))
                                                     ,@(message-for 'newpass2)))
                                             (tr (td ((colspan "2")) (input ((type "submit") (value "Change password")))))
                                             ,@(let ([pw-errors (message-for 'password)])
                                                 (if (null? pw-errors)
                                                     '()
                                                     `((tr (td ((colspan "2")) ,@pw-errors))))))))
                                  (td ((valign "top"))
                                      (form ((action ,k) (method "post") (id "changeEmail"))
                                            (input ((type "hidden") (name "action") (value "setemail")))
                                            (p (b "Change your email address"))
                                            (table
                                             (tr (td "Current address:") 
                                                 (td (b ,(user-email user))))
                                             (tr (td "New address:") 
                                                 (td (input ((type "text") (name "newaddress")))
                                                     ,@(message-for 'newaddress)))
                                             (tr (td "Password:")
                                                 (td (input ((type "password") (name "adrpassword")))
                                                     ,@(message-for 'adrpassword)))
                                             (tr (td ((colspan "2")) (input ((type "submit") (value "Change address"))))))))))))))))
  
  ;; package-update-page : package (listof problem?) -> string -> response
  (define (pkgversion-update-page pkg problems)
    (let* ([general-error-messages (strings->error-xhtml (extract 'general problems))]
           [pv (package->current-version pkg)]
           [major-revision (format "~a.0" (add1 (pkgversion-maj pv)))]
           [minor-revision (format "~a.~a" (pkgversion-maj pv) (add1 (pkgversion-min pv)))])
      (lambda (k)
        (page
         (list "update" (package-name pkg))
         `(,@general-error-messages  
           
           (script
            ,(format
              "
var major = \"~a\";
var minor = \"~a\";

function update(status) {
   var outputNode = document.getElementById('verVal');
   var labelNode = document.getElementById('verLabel');
   if (outputNode != null) {

     // status is either 't', 'f', or null
     if (status == 't') {
        labelNode.innerHTML = \"The new package will be version\";
        outputNode.innerHTML = minor;
     } else if (status == 'f') {
        labelNode.innerHTML = \"The new package will be version\";
        outputNode.innerHTML = major;
     } else {
        labelNode.innerHTML = '';
        outputNode.innerHTML = '';
     }      
   }
}
"            
              major-revision
              minor-revision))
           
           (form 
            ((action ,k) (method "post") (enctype "multipart/form-data"))
            (table 
             (tr (td "Package to use: ") (td (input ((type "file") (name "file")))))
             (tr (td "Backwards-compatible update?") 
                 (td 
                  (select ((name "minor")
                           (onchange "update(this.options[this.selectedIndex].value);"))
                          (option " ")
                          (option ((value "t")) "Yes")
                          (option ((value "f")) "No"))))
             (tr (td (span ((id "verLabel")) nbsp))
                 (td (b (span ((id "verVal")) nbsp))))
             (tr (td ((colspan "2")) (input ((type "submit") (value "Update package")))))))
           (script "update(null);"))))))
  
  (define (do-add-package request)
    (let* ([valid-categories (get-category-names)]
           [valid-repositories (get-all-repositories)]
           [file-bytes (get request 'file)]
           [filename-bytes (get-filename request 'file)])
      (let ([package-name (bytes->string/utf-8 filename-bytes)])
        (create-package user package-name file-bytes))))
  
  ;; do-package-update : package -> void
  ;; manages a user-submitted update to the given package.
  (define (do-package-update pkg)
    (let loop ([problems '()])
      (let* ([request (send/suspend/doctype (pkgversion-update-page pkg problems))] 
             [file-contents (get request 'file)]
             [minor-update?/list (get-all request 'minor)])
        (cond
          [(not (and (pair? minor-update?/list) 
                     (null? (cdr minor-update?/list))
                     (member (car minor-update?/list) '("t" "f"))))
           (loop `((general 
                    ,(string-append "Please select whether you are submitting a minor "
                                    "or a major update (see the PLT Scheme Help Desk "
                                    "for more discussion of the difference between minor "
                                    "and major updates)"))))]                    
          [else
           (let ([minor-update? (string=? (car minor-update?/list) "t")])
             (with-handlers ([exn:fail:bad-package? (λ (e) (loop `((general (span ,@(exn:fail:bad-package-xexprs e))))))])
               (update-package user pkg minor-update? file-contents)))]))))
  
  (define (do-pkg-edit pkg)
    (let ([repositories (get-all-repositories)])
      (let loop ([problems '()])
        (let* ([req (send/suspend/demand (pkg-edit-page pkg problems))])
          
          ;; the user may edit a particular package version's metadata or upload a file to
          ;; update any major version series
          (case (string->symbol (get req 'action))
            [(edit)
             (let ([pv (list-ref (package-versions pkg) (string->number (get req 'pv)))])
               (do-pkgversion-edit pv #:pkg pkg))]
            [(update)
             (let* ([maj           (string->number (get req 'maj))]
                    [file-contents (get req 'contents)])
               (with-handlers ([exn:fail:bad-package? (λ (e) (loop `((general (span ,@(exn:fail:bad-package-xexprs e))))))])
                 (update-non-head-package user pkg maj file-contents)))])))))
  
  (define (do-pkgversion-edit pkgversion #:pkg [pkg #f])
    ;; package developers can edit:
    ;;  - package description
    ;;  - release notes
    ;;  - primary file
    ;;  - categories
    ;;  - required core version
    (let* ([pkg (or pkg (get-package-by-id (pkgversion-package-id pkgversion) (user-id user)))]
           [head-revision? (pv=? pkgversion (package->current-version pkg))]
           
           [current-categories (get-package-categories pkg)]
           [req (send/suspend/demand 
                 (pkgversion-edit-page pkg
                                       pkgversion
                                       (package-blurb pkg)
                                       (pkgversion-blurb pkgversion)
                                       (package-homepage pkg)
                                       (pkgversion-default-file pkgversion)
                                       (pkgversion-required-core pkgversion)
                                       (get-category-names)
                                       current-categories
                                       head-revision?))]
           [blurb-string        (get* req 'description  string->string-option)]
           [notes-string        (get* req 'notes        string->string-option)]
           [homepage-string     (get* req 'homepage     string->string-option)]
           [primary-file-string (get* req 'defaultfile  string->string-option)]
           [core-version-string (get* req 'core         string->string-option)]
           
           ;; FIXME: the ui for editing is horrible
           [blurb (and blurb-string (string->xexprs blurb-string))]
           [notes (and notes-string (string->xexprs notes-string))]
           [primary-file 
            (if primary-file-string
                ; i don't know how to get around this;
                ; there are probably problems with
                ; unicode file names but we only allow ascii anyway due to
                ; db limitations so it's probably not a pressing concern
                (path->string
                 (find-relative-path 
                  (pkgversion-src-path pkgversion) 
                  (normalize-path primary-file-string (pkgversion-src-path pkgversion))))
                #f)]
           [core-version (if core-version-string
                             (srfi13:string-trim-both core-version-string)
                             #f)]
           [categories   (map string->number (get-all req 'categories))]
           [repositories (map string->number (get-all req 'repository))])
      (if head-revision?
          (begin
            (update-package-fields! pkg
                                    pkgversion
                                    blurb
                                    homepage-string
                                    notes
                                    primary-file
                                    core-version)
            (reassociate-package-with-categories pkg categories))
          (update-pkgversion-fields! pkgversion
                                     notes
                                     primary-file
                                     core-version))
      (reassociate-pkgversion-with-repositories pkgversion repositories)))
  
  (define (->string v)
    (if v (format "~s" v) " "))
  (define (->string* v)
    (or v ""))
  
  (define (file-in-package? f p)
    (with-handlers ([exn:fail? (λ (e) #f)])
      (let* ([dir (pkgversion-src-path p)]
             [full-file-path (normalize-path f dir)])
        (and (file-exists? full-file-path)
             (prefix? (explode-path dir) (explode-path full-file-path))))))
  
  (define (prefix? l1 l2)
    (cond
      [(null? l1) #t]
      [(null? l2) #f]
      [(not (equal? (car l1) (car l2))) #f]
      [else (prefix? (cdr l1) (cdr l2))]))
  
  
  (define (groupby f items)
    (foldr (λ (t r) 
             (let ([key (f t)])
               (hash-set r key (cons t (hash-ref r key '())))))
           (make-immutable-hasheq '())
           items))
  
  (define (pkg-edit-page pkg context-problems)
    (define ((page-producer problems) k)
      
      (define (pv-group->rows g)
        (let ([maj (car g)]
              [pvs (cdr g)]
              )
          (cons
           `(tr (td "Package version " ,(number->string maj))
                (td 
                 (form ((action ,k) (method "post") (enctype "multipart/form-data"))
                       
                       "Upload a new revision to version " ,(number->string maj) 
                       (input ((type "file") (name "contents")))
                       (input ((type "hidden") (name "maj") (value ,(number->string maj))))
                       (input ((type "hidden") (name "action") (value "update")))
                       (input ((type "submit") (value "upload"))))))
           (map
            (λ (pv+pvidx)
              (let ([pv (car pv+pvidx)]
                    [pvidx (cdr pv+pvidx)])
                `(tr (td ,(number->string (pkgversion-maj pv)) "."
                         ,(number->string (pkgversion-min pv)))
                     (td (a ((href ,(string-append k (format "?action=edit&pv=~a" pvidx))))
                            "[edit metadata]")))))
            pvs))))
      
      (define (versions->table-rows pvs)
        (cond
          [(not pvs)
           (error 'pkg-edit-page "cannot make an edit page for package stubs")]
          [else
           (let* ([indexed-pvs (map cons pvs (build-list (length pvs) values))]
                  [pv-groups (groupby (λ (x) (pkgversion-maj (car x))) indexed-pvs)]
                  [sorted-groups (sort (hash-map pv-groups cons) (λ (a b) (> (car a) (car b))))])
             (append-map pv-group->rows sorted-groups))]))
      
      (with-problems (append context-problems problems)
                     (λ (general-error-messages value-for errors-for)
                       (page
                        (list "edit" (package-name pkg))
                        `(,@general-error-messages
                          (table 
                           ,@(versions->table-rows (package-versions pkg))))))))
    (define demands 
      (all-demands
       (list 
        (field-in 'action '(edit update))
        (or-demand
         (list
          (field-absent 'pv)
          (field-in-range 'pv 0 (length (package-versions pkg))))))))
    
    (make-demand-page page-producer demands))
  
  
  
  
  ;; this page represents the "new style" and is intended for use with send/suspend/demand
  ;; i should probably switch over the others if this works out
  (define (pkgversion-edit-page pkg 
                                pkgversion
                                description
                                notes
                                homepage
                                default-file
                                required-core
                                categories
                                default-categories
                                head-revision?)
    (define default-category-ids (map category-id default-categories))
    (define ((page-producer problems) k)
      (define on-head
        (if head-revision?
            (λ (a b) a)
            (λ (a b) b)))
      
      (with-problems problems
                     (λ (general-error-messages value-for errors-for)
                       (page
                        (list "edit metadata" 
                              (package-name pkg)
                              (format "~a.~a" (pkgversion-maj pkgversion) (pkgversion-min pkgversion)))
                        `(,@(on-head 
                             '()
                             `((div ((class "notice")) "Notice: you are editing the metadata for a version of your package that is not the most current. Some fields can only be edited on the most recent version of a package. You cannot edit those fields here.")))
                          (form 
                           ((action ,k) (method "post"))
                           (table 
                            (tr (td ((valign "top")) "Package")
                                (td (b ,(package-name pkg))))
                            (tr (td ((valign "top")) "Version")       
                                (td (b ,(format "~a.~a" (pkgversion-maj pkgversion) (pkgversion-min pkgversion)))))
                            (tr (td ((valign "top"))  "Description")   
                                (td ,@(on-head
                                       `((textarea ((name "description") (rows "6") (cols "40"))
                                                   ,(let ([submitted-description (value-for 'description (λ (x) x))])
                                                      (if (null? submitted-description)
                                                          (->string description)
                                                          (->string submitted-description))))
                                         ,@(errors-for 'description))
                                       `(,(->string description)))))
                            (tr (td ((valign "top")) "Home page")
                                (td ,@(on-head 
                                       `((input ((type "text") (name "homepage")
                                                               (value ,(or homepage ""))))
                                         ,@(errors-for 'homepage))
                                       `(,(->string* homepage)))))
                            (tr (td ((valign "top")) "Release notes") 
                                (td (textarea ((name "notes") (rows "6") (cols "40"))
                                              ,(let ([submitted-notes (value-for 'notes (λ (x) x))])
                                                 (if (null? submitted-notes)
                                                     (->string notes)
                                                     (->string submitted-notes))))
                                    ,@(errors-for 'notes)))
                            (tr (td ((valign "top")) "Default file")  
                                (td (input ((type "text") (name "defaultfile") 
                                                          (value ,@(let ([submitted-file (value-for 'defaultfile (λ (x) x))])
                                                                     (if (null? submitted-file)
                                                                         (list (->string* default-file))
                                                                         submitted-file)))))
                                    ,@(errors-for 'defaultfile)))
                            (tr (td ((valign "top")) "Required core version")
                                (td (input ((type "text") (name "core") 
                                                          (value ,@(let ([submitted-core (value-for 'core (λ (x) x))])
                                                                     (if (null? submitted-core)
                                                                         (list (->string* required-core))
                                                                         submitted-core)))))
                                    ,@(errors-for 'core)))
                            (tr (td ((valign "top")) "Categories")
                                (td ((valign "top"))
                                    ,@(on-head
                                       `((table 
                                          ,@(let* ([len (length categories)]
                                                   [half (ceiling (/ len 2))]
                                                   
                                                   [inputbox
                                                    (λ (cat)
                                                      `((input ((type "checkbox")
                                                                (name "categories")
                                                                (value ,(number->string (category-id cat)))
                                                                ,@(if (memv (category-id cat) default-category-ids)
                                                                      `((checked "checked"))
                                                                      `())))
                                                        ,(category-name cat)))])
                                              (let loop ([left (srfi1:take categories half)]
                                                         [right (srfi1:drop categories half)])
                                                (cond
                                                  [(and (null? left) (null? right)) '()]
                                                  [(null? right)
                                                   ;; in this case i'm taking advantage of the fact that i know
                                                   ;; that (len left) is either (len right) or (len right) + 1
                                                   (list
                                                    `(tr 
                                                      (td ,@(inputbox (car left)))
                                                      (td ,@(if (null? right) '(nbsp) (inputbox (car right))))))]
                                                  [else
                                                   (cons
                                                    `(tr
                                                      (td ,@(inputbox (car left)))
                                                      (td ,@(inputbox (car right))))
                                                    (loop (cdr left) (cdr right)))])))))
                                       `((ul
                                          ,@(srfi1:filter-map (λ (cat) (if (memv (category-id cat) default-category-ids)
                                                                           `(li ,(category-name cat))
                                                                           #f))
                                                              categories))))))
                            (tr (td ((valign "top")) "Repositories")
                                ,(let ([reps (get-all-repositories)])
                                   `(table
                                     ,@
                                     (map
                                      (λ (r) 
                                        (let ([rid (repository-id r)])
                                          `(tr 
                                            (td (input ((type "checkbox") 
                                                        (name "repository") 
                                                        (value ,(number->string rid))
                                                        ,@(if (memq rid (pkgversion-repositories pkgversion))
                                                              `((checked "checked"))
                                                              '()))))
                                            (td ,(repository-name r)))))
                                      reps))))
                            
                            (tr (td ((colspan "2")) (input ((type "submit") (value "Update"))))))))))))
    
    ;; url-string? : string -> boolean
    ;; determines if the given string is a reasonable homepage url
    ;; [stolen from plt/collects/planet/util.ss; should be factored out into the net collection]
    (define (url-string? s)
      (and (string? s)
           (let ([u (string->url s)])
             (and (url-scheme u)
                  (url-host u)))))
    (define demands
      (all-demands 
       (list
        ; no fields are required, but any that exist have to be properly formatted
        (fields-ascii 'filename 'defaultfile 'homepage 'notes 'description)
        (field-constraint
         (wrap-as-demand-p
          (blank-or legal-core-version?)
          (λ (v) `(core (message "Must be a legal mzscheme version number"))))
         'core)
        (field-constraint
         (wrap-as-demand-p
          (blank-or (λ (filename) (file-in-package? filename pkgversion)))
          (λ (filename) `(defaultfile (message "Must be the name of a file in your package"))))
         'defaultfile)
        (field-constraint
         (wrap-as-demand-p
          (blank-or (λ (url) (url-string? url)))
          (λ (url) `(homepage (message "Must be a URL"))))
         'homepage))))
    
    (make-demand-page page-producer demands))
  
  (define (verify-and-update-address email)
    (begin
      (verify-changed-address email)
      (update-user-email user email)))
  
  ;; this should be merged with the above
  (define (verify-changed-address email)
    (send/suspend/doctype 
     (lambda (k) 
       (when (SEND-EMAILS?)
         (send-mail-message "PLaneT <planet@plt-scheme.org>" 
                            "Please verify your email address"
                            (list email)
                            '()
                            '()
                            (list "Greetings! You (or someone claiming to be you) have requested to change the PLaneT user account "
                                  (format "~a's email address from ~a to this address (~a)."(user-username user) (user-email user) email) 
                                  "If this was you, please visit the following URL: "
                                  ""
                                  (url->string (combine-url/relative (EXTERNAL-URL-ROOT) k))
                                  ""
                                  "within 48 hours. If it was not you, or you do not want to change your address, then please disregard "
                                  "this message."
                                  ""
                                  "Thanks,"
                                  "PLaneT")))
       (page
        '("Confirm email address")
        `((p "You are chaging your email address from " (b ,(user-email user)) " to " (b ,email))
          ,(if (SEND-EMAILS?)
               `(p "To complete the process, please check the email account "
                   (b ,email) " for a message telling you how to proceed.")
               `(p "Click " (a ((href ,k)) "here") " to continue.")))))))
  
  (let loop ([problems '()])
    (with-handlers 
        ([exn:fail? 
          (λ (e) 
            (parameterize ([current-error-port (current-output-port)])
              ((error-display-handler) (format "~a:\n ~a" (current-date-string) (exn-message e)) e)
              (flush-output (current-error-port)))
            (flush-output)
            (loop `(,general-oops)))])
      (let* ([request (send/suspend/nocache (main-loop-page problems))]
             [bindings (request-bindings request)]
             [action (get request 'action)])
        (case (string->symbol action)
          [(update)
           (let ([pkg (get-package-by-id (string->number (get request 'package)) 
                                         (user-id user))])
             (do-package-update pkg)
             (loop '()))]
          [(fulledit)
           (with-handlers ([exn:fail? 
                            (λ (e) 
                              ((error-display-handler) 
                               (format "fulledit: ~a:\n ~a" (current-date-string) (exn-message e))
                               e)
                              (loop `(,general-oops)))])
             (let ([pkg (get-package-by-id (string->number (get request 'package)) 
                                           (user-id user))])
               (do-pkg-edit pkg)
               (loop '())))]
          [(edit)
           (let ([pkgver (get-package-version-by-id (string->number (get request 'pkgversion)) 
                                                    (user-id user))])
             (with-handlers ([exn:fail? 
                              (λ (e) 
                                ((error-display-handler) 
                                 (format "edit: ~a:\n ~a" (current-date-string) (exn-message e))
                                 e)
                                (loop `(,general-oops)))])
               
               (do-pkgversion-edit pkgver)
               (loop '())))]
          [(newpackage)
           (with-handlers ([exn:fail:bad-package? 
                            (λ (e) (loop `((contribute (message ,@(exn:fail:bad-package-xexprs e))))))])
             (do-add-package request)
             (loop '()))]
          [(setpassword)
           (let* ([demands (all-demands 
                            (list
                             (fields-exist '(oldpass newpass1 newpass2))
                             (fields-ascii 'oldpass 'newpass1 'newpass2)
                             (field-lengths>= 5 'newpass1 'newpass2)
                             (field-constraint
                              (wrap-as-demand-p
                               (lambda (pass) (valid-password? user pass))
                               (lambda (pass) `(oldpass (message "Incorrect password"))))
                              'oldpass)
                             (field-constraint
                              (wrap-as-demand-p string=? (lambda (p1 p2) `(password (message "New passwords did not match"))))
                              'newpass1 'newpass2)))]
                  [problems (demands bindings)])
             (cond
               [(null? problems)
                (begin
                  (user-change-password (user-username user) (get request 'newpass1))
                  (update-user-password user (get request 'newpass1))
                  (loop '((general "Password updated."))))]
               [else (loop problems)]))]
          [(setemail)
           (let* ([demands 
                   (all-demands
                    (list
                     (field-exists 'newaddress)
                     (fields-ascii 'newaddress)
                     (field-constraint 
                      (wrap-as-demand-p 
                       email-available? 
                       (λ (e) `(newaddress (message "The address " (b ,e) " already belongs to another account"))))
                      'newaddress)
                     (field-exists 'adrpassword)
                     (field-constraint
                      (wrap-as-demand-p
                       (lambda (pass) (valid-password? user pass))
                       (lambda (pass) `(adrpassword (message "Incorrect password"))))
                      'adrpassword)))]
                  [problems (demands bindings)])
             (cond
               [(null? problems)
                (verify-and-update-address (get request 'newaddress))
                (loop '((general "Email address changed.")))]
               [else
                (loop problems)]))]
          [else
           (loop `((general ,(format "Oops! Bad action: ~a" action))))])))))



;; ============================================================
;; user creation stuff



(define (email-available? e) (not (email-taken? e)))
(define (username-available? u) (not (username-taken? u)))

(define user-creation-demands
  (all-demands
   (list
    (fields-nonblank '(username realname email password1 password2))
    (fields-ascii 'username 'realname 'email 'password1 'password2)
    (field-constraint (wrap-as-demand-p
                       string=? 
                       (λ (a b) '(password1 (message "Passwords did not match"))))
                      'password1 'password2)
    (field-lengths>= 5 'password1 'password2)
    (field-constraint 
     (wrap-as-demand-p 
      email-available? 
      (λ (e) `(email (message "The address " (b ,e) " has already been taken"))))
     'email)
    (field-constraint 
     (wrap-as-demand-p 
      username-available? 
      (λ (u) `(username (message "The user name " (b ,u) " has already been taken"))))
     'username)
    (field-lengths-in 2 63 'username 'realname)
    (field-lengths<= 128 'email))))

;; get-problems-and-defaults : request -> (values (listof problem) (listof problem))
;; gets the problems with this request as a request for a new user account, and 
;; a list of default (okay) form values should something go wrong
(define (get-problems-and-defaults r)
  (values
   (user-creation-demands r)
   '()))

;; ============================================================
;; UTILITY
(define-syntax show-and-tell
  (syntax-rules ()
    [(show-and-tell expr)
     (let ([ans expr])
       (printf "~s ==> ~s\n" 'expr ans)
       ans)]))

(define (current-date-string)
  (date->string (seconds->date (current-seconds)) #t))

(define (string->string-option s)
  (if (= (string-length (srfi13:string-trim-both s)) 0)
      #f
      s))

;; blank-or : (string -> boolean) -> (string option -> boolean)
;; predicate that matches if either the given string is blank, or the given predicate matches
(define (blank-or p)
  (λ (s)
    (let ([v (string->string-option s)])
      (or (not v) (p v)))))

;; string->xexprs : string -> (listof xexpr)
;; if the given string can be parsed into a list of x-expressions, returns that parse;
;; otherwise returns the singleton xexpr of the given string
(define (string->xexprs s)
  (let/ec return
    (let ([ip (open-input-string s)])
      (with-handlers ([(λ (e)
                         (or (exn:fail? e)
                             (exn:invalid-xexpr? e)))
                       (λ (e) (return `((pre ,s))))])
        (let ([exprs (read ip)])
          (when (regexp-match #rx"[^ \t\n\r]" ip)
            ; it just happened that an s-exp was the first thing in the string; there's more though
            (return `((pre ,s))))
          (for-each validate-xexpr exprs)
          (return exprs))))))

(define (legal-core-version? s)
  (core-version-string->code s))

(define general-oops '(general "Oops! An internal error occured. The problem has been logged, but if you have any further information to report, please email planet@plt-scheme.org."))

