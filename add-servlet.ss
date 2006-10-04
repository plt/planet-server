(module add-servlet mzscheme
  
  (require "db.ss" "package-creation.ss" "data-structures.ss" "html.ss" "configuration.ss" "demands.ss" "package-adder.ss")
  (require (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "file.ss")
           (prefix srfi1: (lib "1.ss" "srfi"))
           (prefix srfi13: (lib "13.ss" "srfi")))
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
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
  (define (get-filename request symbol)
    (let* ([raw (request-bindings/raw request)]
           [binding (get-first (lambda (b)
                                 (and (binding:file? b)
                                      (bytes=? (binding-id b) (string->bytes/utf-8 (symbol->string symbol)))))
                               raw)])
      (unless binding
        (error 'get-filename "No file-binding for ~e exists in the given bindings ~s" symbol raw))
      (binding:file-filename binding)))
  
  ;; get-first : (X -> bool) (listof X) -> (union X #f)
  (define (get-first p l) (ormap (lambda (x) (and (p x) x)) l))
      
  (define (start initial-request)
    (let loop ([problems '()])
      (let ([r (send/suspend (LOGIN-PAGE problems))])
        (if (not (exists-binding? 'mode (request-bindings r)))
            (loop '((general "Your browser did not submit the " (b "mode") " binding, which is necessary")))
            (case (string->symbol (get r 'mode))
              [(login) (login r loop)]
              [(create) (create r loop)]
              [(resetpass) (reset-password)] 
              [else (loop '((general "The mode you used doesn't make sense.")))])))))
  
  (define (reset-password)
    (let loop ([problems '()])
      (let ([r (send/suspend (RESET-PASSWORD-PAGE problems))])
        (let* ([demands 
                (all-demands 
                 (fields-nonblank '(username))
                 (field-constraint
                  (wrap-as-demand-p username-taken? (λ (n) `(username (message "No username " (b ,n) " exists"))))
                  'username))]
               [new-problems (demands (request-bindings r))])
          (cond
            [(null? new-problems)
             (let ([user (get-user-record/no-password (get r 'username))])
               (begin
                 ; send an email to the address associated with the account, the followup to which resets the password
                 (send/suspend (PASSWORD-RESET-EMAIL/PAGE user))
                 ; if send-email returns the email recipient got the message and is in control, so go ahead and let them change the password
                 (do-passwordless-reset user)))]
            [else (loop new-problems)])))))
  
  (define (RESET-PASSWORD-PAGE problems)
    (lambda (k)
      (mkhtmlpage
       '("Reset password")
       `((section "Reset your password")
         (p "For security purposes, we cannot send you your password directly. However, we can allow you to choose a new password "
            " as long as you can verify that you control the email address associated with your username.")
         (form ((action ,k) (method "post"))
               (table (tr (td "Username:") (td (input ((type "text") (name "username")))))
                      (tr (td ((colspan "2")) (input ((type "submit") (value "Send a confirmation email")))))))))))
  
  ;; send an email to the given user with a link allowing them to continue, and return a web page indicating this has been done
  (define (PASSWORD-RESET-EMAIL/PAGE user)
    (lambda (k) 
      #;(send-mail-message "PLaneT <planet@plt-scheme.org>" 
                          "A password reset request has been made for this account"
                          (list k)
                          '()
                          '()
                          (list "Greetings! You (or someone claiming to be you) has asked to reset the password for the "
                                "account " (user-username user) " on PLaneT, the PLT Scheme package repository. "
                                "Our records indicate that that account belongs to this email address. If you really want to "
                                "reset your password, then please visit the following URL: "
                                ""
                                k
                                ""
                                "If you do not want to reset the password to your PLaneT account, then please disregard "
                                "this message."
                                ""
                                "Thanks,"
                                "PLaneT"))
       (mkhtmlpage 
        '("Confirm email")
        `((section "Confirmation message sent")
          (p "We have sent an email to the address we have listed as belonging to the user " (b ,(user-username user))". "
             "For security purposes, you must visit the link provided in that message to proceed. If you have further questions, "
             "please contact a PLaneT administrator by emailing planet@plt-scheme.org for help.")
          (p "For testing: " (a ((href ,k)) "click here"))))))   
  
  ;; do-passwordless-reset : user -> void 
  (define (do-passwordless-reset user)
    (let* ([demands
            (field-constraint 
             (wrap-as-demand-p string=? (lambda (a b) "Passwords did not match"))
             'password1 'password2)]
           [PAGE (PASSWORDLESS-RESET-PAGE user)])
      (let loop ([problems '()])
        (let* ([r (send/suspend (PAGE problems))]
               [problems (demands (request-bindings r))])
          (cond
            [(null? problems)
             ; update the user's password and then send to the logged-in state
             (update-user-password user (get r 'password1))
             (main-interaction-loop user)]
            [else (loop problems)])))))
  
  (define ((PASSWORDLESS-RESET-PAGE user) problems)
    (lambda (k)
      (mkhtmlpage
       '("password reset")
       `((p "You have now validated yourself as user " (b ,(user-username user))
            " by responding to email. Please use this form to reset your password.")
         (section "Reset your password")
         (form ((action ,k) (method "post"))
               (table
                (tr (td "Username:")     (td (b ,(user-username user))))
                (tr (td "New password:") (td (input ((type "password") (name "password1")))))
                (tr (td "Again:")        (td (input ((type "password") (name "password2")))))
                (tr (td ((colspan "2")) (input ((type "submit") (value "Reset password")))))))))))
  
  
  ;; ============================================================
  ;; logged-in-user stuff
           
    
  (define (login r fail-with)
    (let ([login (get r 'login)]
          [pw    (get r 'pw)])
      (cond
        [(get-user-record login pw)
         => (lambda (user) (main-interaction-loop user))]
        [else 
         (fail-with `((general "Incorrect username or password.") (login (value ,login))))])))
  
  (define (main-interaction-loop user)
 
    (define (do-add-package)
      (let* ([valid-categories (get-category-names)]
             [valid-repositories (get-all-repositories)]
             [request (send/suspend (package-add-page valid-categories valid-repositories))]
             [file-bytes (get request 'file)]
             [filename-bytes (get-filename request 'file)]
             [categories/strs (get-all request 'category)]
             [repositories/strs (get-all request 'repository)]
             [blurb (get request 'blurb)])
        (let ([package-name (bytes->string/utf-8 filename-bytes)]
              [categories 
               (map (lambda (catstr)
                      (get-first
                       (lambda (cat) (= (category-id cat) (string->number catstr)))
                       valid-categories))
                    categories/strs)]
              [repositories 
               (map (lambda (repstr)
                      (get-first
                       (lambda (rep) (= (repository-id rep) (string->number repstr)))
                       valid-repositories))
                    repositories/strs)])
          (create-package user package-name file-bytes categories blurb repositories))))

    (define (do-package-update pkg)
      (let* ([repositories (get-all-repositories)]
             [valid-ids (map repository-id repositories)])
        (let loop ([problems '()])
          (let* ([request (send/suspend (package-update-page pkg repositories problems))] 
                 [file-contents (get request 'file)]
                 [minor-update? (exists-binding? 'minor (request-bindings request))]
                 [repository-id-strings (extract-bindings 'repository (request-bindings request))]
                 [repository-ids (map string->number repository-id-strings)])
            (cond
              [(null? repository-ids)
               (loop `((general "Please select at least one repository for your package")))]
              [(not (andmap (lambda (x) (memv x valid-ids)) repository-ids))
               (loop `((general "Illegal repository selected")))]
              [else
               (update-package user pkg minor-update? file-contents 
                               ;; the map below converts repository ids to repository data structures
                               (map 
                                (lambda (id) 
                                  (srfi1:find 
                                   (lambda (r) (= id (repository-id r)))
                                   repositories))
                                repository-ids))])))))
    
    (define this-user-main-loop-page (main-loop-page user))
    (let loop ()
      (let* ([request (send/suspend this-user-main-loop-page)]
             [bindings (request-bindings request)]
             [action (get request 'action)])
        (case (string->symbol action)
          [(refresh) (loop)]
          [(update)
           (let ([pkg (get-package-by-id (string->number (get request 'package)) 
                                         (user-id user))])
             (do-package-update pkg)
             (loop))]
          [(newpackage)
           (do-add-package)
           (loop)]
          [(setpassword)
           (let* ([demands (all-demands 
                            (fields-exist '(oldpass newpass1 newpass2))
                            (field-constraint
                             (wrap-as-demand-p
                              (lambda (pass) (valid-password? user pass))
                              (lambda (pass) `(general "Incorrect password")))
                             'oldpass)
                            (field-constraint
                             (wrap-as-demand-p string=? (lambda (p1 p2) `(general "New passwords did not match")))
                             'newpass1 'newpass2))]
                  [problems (demands bindings)])
             (cond
               [(null? problems)
                (begin
                  (update-user-password user (get request 'newpass1))
                  ;; TODO: make a message saying password was updated
                  (loop))]
               ;; TODO: update page to handle errors in the good style
               [else (loop)]))]
          [else
           (send/suspend 
            (lambda (k)
              (mkhtmlpage (list "error") 
                          `((p "Oops! Bad action: " ,action)
                            (a ((href ,k)) "continue")))))
           (loop)]))))
  
  ;; ============================================================
  ;; user creation stuff
  
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
           (let ([user (create-new-user username realname email password)])
             (main-interaction-loop user)))])))
  
  (define (email-available? e) (not (email-taken? e)))
  (define (username-available? u) (not (username-taken? u)))
  
  (define user-creation-demands
    (all-demands
     (fields-nonblank '(username realname email password1 password2))
     (field-constraint (wrap-as-demand-p
                        string=? 
                        (λ (a b) '(general "Passwords did not match.")))
                       'password1 'password2)
     (field-constraint (wrap-as-demand-p email-available? (λ (e) `(email (message "The address " (b ,e) " has already been taken"))))
                       'email)
     (field-constraint (wrap-as-demand-p username-available? (λ (u) `(username (message "The user name " (b ,u) " has already been taken"))))
                       'username)
     (field-lengths<= 63 'username 'realname)))
  
  ;; get-problems-and-defaults : request -> (values (listof problem) (listof problem))
  ;; gets the problems with this request as a request for a new user account, and 
  ;; a list of default (okay) form values should something go wrong
  (define (get-problems-and-defaults r)
    (values
     (show-and-tell (user-creation-demands r))
     '()))
  
  
  
  (define (verify-email-address email)
    (send/suspend 
     (lambda (k) 
       #;(send-mail-message "PLaneT <planet@plt-scheme.org>" 
                          "Please verify your email address"
                          (list k)
                          '()
                          '()
                          (list "Greetings! You (or someone claiming to be you) have signed up for an account "
                                "with PLaneT, the PLT Scheme package repository. To verify your email address, "
                                "please visit the following URL: "
                                ""
                                k
                                ""
                                "If you do not want to create an account with PLaneT, then please disregard "
                                "this message."
                                ""
                                "Thanks,"
                                "PLaneT"))
       (mkhtmlpage 
        '("Confirm email address")
        `((p "Thank you for creating an account! To complete the registration process, please check the email account "
             (b ,email) " for a message telling you how to proceed.")
          (p "For testing: " (a ((href ,k)) "click here")))))))
  
  
  ;; ============================================================
  ;; HTML PAGES
  
  
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
      (mkhtmlpage
       (list "contribute")
       `((p "Thanks for deciding to contribute a package to PLaneT! You can either create an account using the form below, or, if you already have 
an account, log in directly.")
         (section "Log in")
         ,@general-error-messages
         (div 
          (div ((id "logIn"))
               (form ((action ,k) (method "post"))
                     (input ((type "hidden") (name "mode") (value "login")))
                     (p "Already have a user account? Then log in here.")
                     (table
                      (tr (td "User name")
                          (td (input ((name "login") 
                                      (type "text")
                                      ,@(value-for 'login))) ,@(message-for 'login)))
                      (tr (td "Password")
                          (td (input ((name "pw") (type "password")))))
                      (tr (td ((colspan "2")) (input ((type "submit") (value "Log in")))
                              (br)
                              (small (a ((href ,(string-append k "?mode=resetpass"))) "I forgot my password")))))))
          
          (div ((id "createUser"))
               (form ((action ,k) (method "post"))
                     (input ((type "hidden") (name "mode") (value "create")))
                     (p "Need a user account? Create one now.")
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
  
  ;; FIXME: currently the user just gets packages from the default repository. It should be from _any_ repository, but
  ;; with that info clearly displayed and without any duplicates.
  (define (main-loop-page user)
    (lambda (k)
      (define packages (user->packages user (DEFAULT-REPOSITORY))) 
      (define (package->rows pkg)
        (let ([v (car (package-versions pkg))])
          `((tr ((class "pkgStats")) 
                (td ,(package-name pkg)) 
                (td ,(format "~a.~a"
                             (pkgversion-maj v)
                             (pkgversion-min v)))
                (td (a ((href ,(string-append k "?action=update&package=" (number->string (package-id pkg)))))
                       "update this package")))
            (tr ((class "pkgBlurb"))
                (td ((colspan "3")) ,(if (package-blurb pkg)
                                         (make-cdata #f #f (package-blurb pkg))
                                         "[no package description]")))
            (tr ((class "pkgVersionBlurb"))
                (td ((colspan "3")) ,(if (pkgversion-blurb v)
                                         (make-cdata #f #f (pkgversion-blurb v))
                                         "[no version notes]"))))))
      (mkhtmlpage
       (list `("Home" ,(DISPLAY-URL-ROOT)) "Your packages")
       `((section "Contribute packages")
         (form ((action ,k) (method "post"))
               (p (a ((href ,(string-append k "?action=newpackage"))) "create a new package"))    
               ,@(cond
                   [(null? packages) '()]
                   [else
                    `((p "These are your packages:")
                      (table ((id "yourPackages")) ,@(apply append (map package->rows packages))))])
               (input ((type "hidden") (name "action") (value "refresh")))
               (input ((type "submit") (value "Refresh"))))
         (section "Manage your account")
         (form ((action ,k) (method "post"))
           (input ((type "hidden") (name "action") (value "setpassword")))
           (p (b "Change your password"))
           (table
            (tr (td "Old password:") (td (input ((type "password") (name "oldpass")))))
            (tr (td "New password:") (td (input ((type "password") (name "newpass1")))))
            (tr (td "Again:") (td (input ((type "password") (name "newpass2")))))
            (tr (td ((colspan "2")) (input ((type "submit") (value "Change password")))))))))))
  
  (define (package-add-page categories repositories)
    (lambda (k)
      (mkhtmlpage
       (list "add a new package")
       `((form ((action ,k) (method "post") (enctype "multipart/form-data"))
               
           (table
            (tr (td "File") (td (input ((type "file") (name "file")))))
            (tr (td "Description") (td (textarea ((name "blurb")) " ")))
            (tr (td "Categories (check all that apply)") (td ""))
            ,@(map
               (lambda (cat)
                 `(tr (td "") (td (input ((type "checkbox") (name "category") (value ,(number->string (category-id cat)))))
                                  ,(category-name cat))))
               categories)
            (tr (td "Which repositories is this package compatible with?") (td ""))
               ,@(map
                  (lambda (rep)
                    `(tr (td "") (td (input ((type "checkbox") (name "repository") (value ,(number->string (repository-id rep)))
                                                               ,@(if (= (repository-id rep) (DEFAULT-REPOSITORY))
                                                                     `((selected "selected"))
                                                                     '())))
                                     ,(repository-name rep))))
                  repositories)
            (tr (td ((colspan "2")) (input ((type "submit")))))))))))
  
  (define (package-update-page pkg repositories problems)
    (let ([general-error-messages (strings->error-xhtml (extract 'general problems))])
      (lambda (k)
        (mkhtmlpage
         (list "update" (package-name pkg))
         `(,@general-error-messages  
             (form 
              ((action ,k) (method "post") (enctype "multipart/form-data"))
              (table 
               (tr (td "Package to use: ") (td (input ((type "file") (name "file")))))
               (tr (td "Backwards-compatible update?") (td (input ((type "checkbox") (name "minor")))))
               (tr (td "Which repositories is this update compatible with?") (td ""))
               ,@(map
                  (lambda (rep)
                    `(tr (td "") (td (input ((type "checkbox") (name "repository") (value ,(number->string (repository-id rep)))
                                                               ,@(if (= (repository-id rep) (DEFAULT-REPOSITORY))
                                                                     `((selected "selected"))
                                                                     '())))
                                     ,(repository-name rep))))
                  repositories)
               (tr (td ((colspan "2")) (input ((type "submit"))))))))))))
      
  
  (define (show-and-tell v)
    (printf "~s\n" v)
    v))