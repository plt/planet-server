(module user-create-servlet mzscheme
  
  (require "db.ss" "package-creation.ss" "data-structures.ss" "html.ss" "configuration.ss")
  (require (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "file.ss")
           (prefix srfi1: (lib "1.ss" "srfi"))
           (prefix srfi13: (lib "13.ss" "srfi")))
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (startup)
  
  (define (get request symbol)
    (extract-binding/single symbol (request-bindings request)))
  
  (define (start req)
    (let loop ()
      (let ([r (send/forward LOGIN/CREATE-USER-PAGE)])
        (cond
          [(not (binding-exists? (request-bindings r) 'mode)) (loop)]
          [(eq? (get r 'mode) 'login)
           (get-user-info/loop r)]
          [(eq? (get r 'mode) 'create)
           
    
    
    (get-user-info/loop req))
  
         
  
  
  ;; the list of things that will stop a new user account from being created
  (define-struct problem ())
  (define-struct (already-taken-username-problem problem) ())
  (define-struct (passwords-do-not-match-problem problem) ())
  (define-struct (email-taken-problem problem) ())
  (define-struct (nonexistant-field-problem problem) (field))
  
  (define problems
    (list
     (lambda (r) 
       (cond
         [(not (exists-binding? 'username (request-bindings r)))
          (make-nonexistant-field-problem 'username)]
         [(username-taken? (srfi13:string-trim-both (get r 'username)))
          (make-already-taken-username-problem)]
         [else #f]))
     (lambda (r)
       (cond
         [(not (exists-binding? 'password1 (request-bindings r)))
          (make-nonexistant-field-problem 'password1)]
         [(not (exists-binding? 'password2 (request-bindings r)))
          (make-nonexistant-field-problem 'password2)]
         [(not (string=? (get r 'password1) (get r 'password2)))
          (make-passwords-do-not-match-problem)]
         [else #f]))
     (lambda (r)
       (cond
         [(not (exists-binding? 'email (request-bindings r)))
          (make-nonexistant-field-problem 'email)]
         [(email-taken? (srfi13:string-trim-both (get r 'email)))
          (make-email-taken-problem)]
         [else #f]))))
  
  (define (get-user-info/loop req)
    (let* ([problems (srfi1:filter-map (lambda (problem) (problem req)) problems)])
      (cond
        [(pair? problems)
         (get-user-info/loop (send/forward (USER-CREATE-PAGE problems)))]
        [else
         (let ([username (srfi13:string-trim-both (get req 'username))]
               [realname (srfi13:string-trim-both (get req 'realname))]
               [email (srfi13:string-trim-both (get req 'email))]
               [password (get req 'password1)])
           (verify-email-address email)
           (create-new-user username realname email password)
           (send/finish FINISHED-PAGE))])))
  
  (define (USER-CREATE-PAGE problems)
    (lambda (k)
      (mkhtmlpage 
       '("Create a user account")
       `((p "On this page you can create an account for contributing packages to PLaneT.")
         (form ((action ,k) (method "post"))
               (table
                (tr (td "Desired username")
                    (td (input ((type "text") (name "username")))))
                (tr (td "Your real name")
                    (td (input ((type "text") (name "realname")))))
                (tr (td "Your email address")
                    (td (input ((type "text") (name "email")))))
                (tr (td "A password")
                    (td (input ((type "password") (name "password1")))))
                (tr (td "Password again")
                    (td (input ((type "password") (name "password2"))))))
               (input ((type "submit"))))))))
  
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
  
  (define FINISHED-PAGE
    (mkhtmlpage '("User created")
                '((p "Your user account has been created. Thank you!")))))
