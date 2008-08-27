
#lang scheme/base

(require  (lib "contract.ss") 
          (lib "md5.ss")
          (lib "list.ss")
          (lib "class.ss")
          (lib "scheme/system.ss")
          (planet "spgsql.ss" ("schematics" "spgsql.plt" 2 3))
          net/sendmail
          srfi/13
          "../data-structures.ss"
          "../db.ss"
          "trac-admin.ss"
          "xmlrpc/xml-rpc.ss")          

(provide/contract 
 [get-all-packages (-> (listof package?))]
 [add-all-packages-to-trac (->  void)]
 [add-package-to-trac! (-> cons? void)]
 [add-all-users-to-trac (-> void)]
 [compare-users (-> (or/c void (listof string? )))]
 [compare-packages (-> void)]
 [remove-repeats (-> list? list? list?)])

;======================================================================================================================
;For trac package management

;for adding all current packages in planet to trac
;listof package->void
(define (add-all-packages-to-trac)
  (let* ([all-packages (get-all-packages)])
    (map (lambda(x)
           (add-package-to-trac!  x)) all-packages)))

;(cons string? string?) -> void
(define (add-package-to-trac! pack)
  (add-component (car pack) (cdr pack)))

;void->listof package
;; get-all-packages : natural-number/c (union repository? natural-number/c) -> (listof package?)
;; gets all packages from the planet database
(define (get-all-packages)
  (let* ([packages   (get-all-packages-no-version)]
         [no-repeats (remove-repeats packages '())])
  no-repeats))

(define (remove-repeats to-see have-seen)
  (if (empty? to-see)
      have-seen
      (if (or (member (first to-see) have-seen))
          (remove-repeats (rest to-see) have-seen)
          (remove-repeats (rest to-see) (cons (first to-see) have-seen)))))


;========================================================================================================================
;Compare PLaneT users/packages with Trac users/components and bring them into sync if they aren't already
(define (compare-users)
  (let* ([trac-users (file-parser "/local/password/users.txt")]
         [plt-users-email (map (lambda (x) (first x)) (get-all-user-email))]
         [plt-users (foldl (lambda(x y ) (cons x y)) '() plt-users-email)]
         [extra-trac-users '()])
    (map (lambda(x)
           (let *([username (first x)]
                  [email (first (rest x))]
                  [rand-pass (random-password)])
             (when (not (user-exists? username))
                 (and (user-add (first x) rand-pass)
                      (send-notification-email email rand-pass))))) plt-users-email)
    (map (lambda(x)
           (when (not (member x plt-users))
             (cons x extra-trac-users))) trac-users)))

(define (compare-packages)
  (let* ([plt-users-email (get-all-user-email)]
         [plt-users-filtered (map (lambda(x) (first x) ) (filter (lambda (x) (not (equal?  (first x)  "flaviocruz"))) plt-users-email))]
         [plt-users (map (lambda(x) (get-user-record/no-password x)) plt-users-filtered)] 
;Here is the problem...                                                                               
        [plt-packages (foldl (lambda(x y) (append (user->packages x) y)) '() plt-users)]
        [loop (let ([component (list-component)])
                            (let loop()
                              (let ([line (read-line component)])
                                (cond  
                                  [(eof-object? line)
                                   (begin
                                     (close-input-port component)
                                     empty)]
                                  [(or (string=? "" line) 
                                       (string=? "Name                         Owner        " line)
                                       (string=? "------------------------------------------" line))
                                   (loop)]
                                  [else (cons line (loop))]))))]
        
        [trac-components (map component-parse loop)]) 
    (map (lambda(x) (when (not (member x trac-components))
                      (close-input-port (add-component (package-name x) (package-owner x))))) plt-packages)
    (foldr (lambda(x y) (when (not (member x plt-packages))
                      (cons x y))) '() trac-components)))
         
;=======================================================================================================================
;Helper parsing functions

(define (component-parse item)
  (let* ([index (string-index item #\.)]
         [parsed (substring item 0 (+ 4 index))])
    parsed))
        
         
  
(define (file-parser file)
  (let *([textfile (open-input-file file)])
   (let loop()
     (let *([line (read-line textfile)])
       (if (eof-object? line)
           (begin 
             (close-input-port textfile)
             empty)
           (let* ([index (string-index line #\:) ]
                  [parsed (substring line 0  index)])
            (cons parsed (loop))))))))
    
           

;==========================================================================================================================
;for Trac user management

;; random-password : -> string
  ;; generates a random password to seed the database
  (define (random-password)
    (list->string (build-list 8 (λ (_) (integer->char (random-int-in 32 127))))))
  
  
  ;; random-int-in : int int -> int
  ;; delivers a random int in the range [lo,hi] 
  (define (random-int-in lo hi)
    (+ (random (- hi lo)) lo))
  
;add all users to trac
;NOTE: the password file must alread exist
;void->void
(define (add-all-users-to-trac)
 (let ([users  (get-all-user-email)])
    (map (lambda(x)
           (let ([username (first x)]
                 [email (first (rest x))])
           (when (not (user-exists?  username))
             (let ([random-pass (random-password)])
               (user-add username random-pass)
               (list username random-pass)
                      #;   (send-notification-email email random-pass)))
                           )) users)))

(define (send-notification-email email pass)
  (send-mail-message "PLaneT <planet@plt-scheme.org>" 
                     "PLaneT has a new Bug-Tracker"
                     (list email)
                     '()
                     '()
                     (list (string-append "Greetings! PLaneT has just been set up to use Trac, an issue-tracking system. We need you to update your password. Please go to planet.plt-scheme.org/, login using your username, and change your password at your earliest convenience.")
                                                      
                           "Thanks,"
                           "PLaneT")))