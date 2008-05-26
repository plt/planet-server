#lang scheme/base

(require (planet schematics/schemeunit:2:10/test)
         (planet schematics/schemeunit:2:10/text-ui)
         
         scheme/system
         scheme/list
         xml/xml
         
         planet/util
         planet/config
         
         web-server/private/request-structs  ;; for generating a dummy req for use in testing web stuff
         net/url
         
         "db.ss"
         "data-structures.ss"
         "display.ss")

(provide (all-defined-out))

;; the testing database has a specific setup that exercises weird cases,
;; so we set it up the same way each time
(define (initialize-testing-database!)
  #;(system "/local/pgsql/bin/psql -f testing-db.sql")
  (void))

(define (files=? f1 f2)
  (and 
   ;; short-circuit if this test fails, since it's much quicker than the next test
   (= (file-size f1) (file-size f2))
   
   (let ([i1 (open-input-file f1)]
         [i2 (open-input-file f2)]
         [buf1 (make-bytes 65536)]
         [buf2 (make-bytes 65536)])
     (let loop ()
       (let ([len1 (read-bytes! buf1 i1)]
             [len2 (read-bytes! buf2 i2)])
         (cond
           [(not (equal? len1 len2)) #f]
           [(eof-object? len1) #t]
           [else (loop)]))))))
   
   
(define (pkg-path owner pkg maj min)
  (build-path "/local/planet/archives" owner pkg (number->string maj) (number->string min) pkg))

(define (pv->summary pv)
  (list (pkgversion-maj pv) (pkgversion-min pv)))

(define-simple-check (check-package actual expected-owner expected-package-name)
  (and
   (package? actual)
   (string=? (package-owner actual) expected-owner)
   (string=? (package-name actual) expected-package-name)))

(define-check (check-package-version actual expected)
  (let-values ([(id pkg-id maj min reps) (apply values expected)])
    (define (checkprop name accessor expected)
      (let ([received (accessor actual)])
        (unless (equal? received expected)
          (with-check-info (['name received]) (fail-check)))))
    (checkprop 'id pkgversion-id id)
    (checkprop 'package-id pkgversion-package-id pkg-id)
    (checkprop 'maj pkgversion-maj maj)
    (checkprop 'min pkgversion-min min)
    (checkprop 'repositories pkgversion-repositories reps)))

(define-check (check-get-matching-packages-results requester-core-version pkgowner pkgname maj minlo minhi expected-pv-summaries)
  (let*-values ([(pvs _) (get-matching-packages requester-core-version pkgowner pkgname maj minlo minhi)]
                [(actual-pv-summaries) (map pv->summary pvs)])
    (for-each 
     (λ (actual-pv-summary)
       (unless (findf (λ (expected-pv-summary) (equal? expected-pv-summary actual-pv-summary)) expected-pv-summaries)
         (with-check-info (['unexpected-pv-summary actual-pv-summary])
           (fail-check))))
     actual-pv-summaries)
    (for-each
     (λ (expected-pv-summary)
       (unless (findf (λ (actual-pv-summary) (equal? expected-pv-summary actual-pv-summary)) actual-pv-summaries)
         (with-check-info (['expected-pv-summary expected-pv-summary])
           (fail-check))))
     expected-pv-summaries)))
       
(define-check (check-server-gives-file require-spec expected-maj expected-min)
  (let* ([owner (car require-spec)]
         [pkgname (cadr require-spec)]
         [expected-result-file (pkg-path owner pkgname expected-maj expected-min)]
         [spec (apply get-package-spec require-spec)]
         [results (download-package spec)])
    (with-check-info (['actual-server-result results])
        (unless (pair? results)
           ; server internal error
          (fail-check))
        (unless (car results)
          ; missing package
          (fail-check))
        
        (let-values ([(filename maj min) (apply values (cdr results))])
          (dynamic-wind
           void
           (λ () 
             (unless (and (= expected-maj maj)
                          (= expected-min min)
                          (files=? expected-result-file filename))
               (fail-check)))
           (λ () (delete-file filename)))))))

(define-check (check-server-has-no-match require-spec)
  (let* ([owner (car require-spec)]
         [pkg (cadr require-spec)]
         [spec (apply get-package-spec require-spec)]
         [results (download-package spec)])
    (with-check-info (['actual-server-result results])
     (dynamic-wind
      void
      (λ ()
        (unless (and (pair? results) (not (car results)))
          (fail-check)))
      (λ () 
        (when (car results)
          (delete-file (cadr results))))))))

(define real-url (HTTP-DOWNLOAD-SERVLET-URL))

(define db.ss-tests
  (test-suite "db tests"
    
    ;[username-taken? (-> string? boolean?)]
    (test-suite "username-taken?"
      (test-equal? "1" (username-taken? "jacobm") #t)
      (test-equal? "2" (username-taken? "planet") #t)
      (test-equal? "3" (username-taken? "not-a-real-user") #f))
    
    ;[email-taken? (-> string? boolean?)]
    (test-suite "email-taken?"
      (test-equal? "1" (email-taken? "test@example.com") #t)
      (test-equal? "2" (email-taken? "not-taken@taken.not") #f))
    
    ;;[create-new-user (-> string? string? string? string? user?)]
    (test-suite "create-new-user")

    ;;[get-user-record (-> string? string? (union user? false/c))]
    (test-suite "get-user-record"
      (test-equal? "1" (get-user-record "test" "not the password") #f)
      (test-equal? "2" (user? (get-user-record "test" "test")) #t))
    
    ;;[log-user-in (-> user? string?)]
    ;;[get-logged-in-user-from-passcode (-> string? string? (union user? false/c))]
    ;;[log-user-out (-> user? void?)]
    (test-suite "get-logged-in-user-from-passcode / log-user-in / log-user-out"
      (test-true "1"
        (let* ([u        (get-user-record/no-password "test")]
               [passcode (log-user-in u)]
               [u2       (get-logged-in-user-from-passcode "test" passcode)]
               [bad      (get-logged-in-user-from-passcode "test" (string-append passcode "000"))]
               [_        (log-user-out u)]
               [bad2     (get-logged-in-user-from-passcode "test" passcode)])
          (and (equal? u u2)
               (not bad)
               (not bad2)))))
    
    ;;[get-user-record/no-password (-> string? (union user? false/c))]
    (test-suite "get-user-record/no-password"
      (test-equal? "1" (user-realname (get-user-record/no-password "jacobm")) "Jacob Matthews")
      (test-equal? "2" (user-realname (get-user-record/no-password "test"))   "Testy McTestTest")
      (test-equal? "3" (get-user-record/no-password "not-here") #f))
    
    ;; [valid-password? (user? string? . -> . boolean?)]
    (test-suite "valid-password?"
      (test-equal? "1" (valid-password? (get-user-record/no-password "jacobm") "not-my-real-password") #f)
      (test-equal? "2" (valid-password? (get-user-record/no-password "test") "test") #t))
    
    ;;[update-user-email (user? string? . -> . void?)]
    (test-suite "update-user-email"
      (test-true "1"
        (let* ([u (get-user-record/no-password "test")]
               [old-email (user-email u)]
               [new-email (string-append "new-" old-email)])
          (update-user-email u new-email)
          (and (string=? (user-email u) new-email)
               (string=? (user-email (get-user-record/no-password "test")) new-email)
               (begin 
                 (update-user-email u old-email)
                 (and 
                  (string=? (user-email u) old-email)
                  (string=? (user-email (get-user-record/no-password "test")) old-email)))))))
    
    ;; [update-user-password (user? string? . -> . void?)]
    (test-suite "update-user-password"
      (test-true "1"
        (let ([u (get-user-record/no-password "test")])
          (update-user-password u "newpassword")
          (and
           (valid-password? u "newpassword")
           (begin
             (update-user-password u "test")
             (valid-password? u "test"))))))
    
    ;;[user->packages (opt-> (user?) ((union (listof natural-number/c) false/c))  (listof package?))]
    (test-suite "user->packages"
      (test-true "1"
       (let* ([u (get-user-record/no-password "test")]
              [pkgs (user->packages u)])
         ;; basic sanity check
         (andmap
          (λ (pkg) (apply pv>? (package-versions pkg)))
          pkgs))))
    
    ;;[get-category-names (-> (listof category?))]
    (test-suite "get-category-names"
      (test-equal? "1" (map category-name (get-category-names))
        '("Development Tools"
          "Networking and Protocols"
          "Graphics and Audio"
          "XML-Related"
          "Data Structures and Algorithms"
          "Input/Output and Filesystem"
          "Mathematical and Scientific"
          "Hardware/Operating System-Specific Tools"
          "Textual and Graphical User Interface"
          "Metaprogramming Tools"
          "PLaneT-Related"
          "Miscellaneous")))
    
    ;; [add-package-to-db! (user? string? (or/c (listof xexpr?) false/c) (or/c string? false/c) . -> . package?)]
    (test-suite "add-package-to-db!")
    
    ;;[get-package-listing (natural-number/c . -> . (listof category?))]
    (test-suite "get-package-listing")
    
    ;; [get-matching-packages
    ;;  (opt->*
    ;;   (string? string? string? (union natural-number/c false/c) natural-number/c (union natural-number/c false/c))
    ;;   (repository?)
    ;;   ((listof pkgversion?) boolean?))]
    (test-suite "get-matching-packages"
      (test-case "1"
        (check-get-matching-packages-results "3.99.0.0" "planet" "test-connection.plt" 1 0 #f
                                             '((1 0))))
      (test-case "2"
        (check-get-matching-packages-results "3.99.0.0" "planet" "test-connection.plt" 1 0 0
                                             '((1 0))))
      (test-case "3"
        (check-get-matching-packages-results "370" "planet" "test-connection.plt" 1 0 #f
                                             '((1 0))))
      (test-case "4"
        (check-get-matching-packages-results "4.0" "planet" "test-connection.plt" 1 0 #f
                                             '((1 0))))
      (test-case "5"
        (check-get-matching-packages-results "5.8" "planet" "test-connection.plt" 1 0 #f
                                             '()))
      (test-case "6"
        (check-get-matching-packages-results "5.8" "nobody" "no-file.plt" 1 0 #f
                                             '()))
      (test-case "7"
        (check-get-matching-packages-results "3.99.0.0" "planet" "test-connection.plt" 1 1 #f
                                             '()))
      (test-case "8"
        (check-get-matching-packages-results "3.99.0.0" "planet" "test-connection.plt" 1 2 3
                                             '())))
    
    ;;[get-package (opt-> (string? string?) (boolean?) (union package? false/c))]
    (test-suite "get-package"
      (test-case "1" (check-package (get-package "planet" "test-connection.plt") "planet" "test-connection.plt"))
      (test-case "2" (check-package (get-package "jacobm" "crypto.plt") "jacobm" "crypto.plt"))
      (test-case "3" (check-package (get-package "planet" "test-connection.plt" #f) "planet" "test-connection.plt"))
      (test-case "4" (check-package (get-package "jacobm" "crypto.plt" #f) "jacobm" "crypto.plt"))
      (test-case "5" (check-package (get-package "planet" "test-connection.plt" #t) "planet" "test-connection.plt"))
      (test-case "6" (check-package (get-package "jacobm" "crypto.plt" #t) "jacobm" "crypto.plt"))
      
      (test-equal? "7" (get-package "planet" "crypto.plt" #t) #f)
      (test-equal? "8" (get-package "jacobm" "test-connection.plt" #t) #f)
      (test-equal? "9" (get-package "asdfasdfasdfas" "dfasdfasdfij") #f)
      
      (test-equal? "10" (get-package "jacobm" "foof-loop.plt") #f)
      (test-equal? "11" (get-package "jacobm" "foof-loop.plt" #f) #f)
      (test-case "12" (check-package (get-package "jacobm" "foof-loop.plt" #t) "jacobm" "foof-loop.plt")))
    
    ;;[get-package-by-id (-> natural-number/c natural-number/c (union package? false/c))]
    (test-suite "get-package-by-id"
      (test-case "1" (check-package (get-package-by-id 26 18) "planet" "test-connection.plt"))
      (test-case "2" (check-package (get-package-by-id 187 34) "jacobm" "crypto.plt"))
      (test-equal? "3" (get-package-by-id 187 80) #f)
      (test-equal? "4" (get-package-by-id 26 34) #f))
    
    ;;[get-package-version-by-id (-> natural-number/c natural-number/c (union pkgversion? false/c))]
    (test-suite "get-package-version-by-id"
      (test-case "1"
        (check-package-version 
         (get-package-version-by-id 80 18)
         (list 80 26 1 0 (list 2 3))))
      (test-false "2" (get-package-version-by-id 80 20)))
  
    ;;[reassociate-package-with-categories (package? (listof (or/c category? natural-number/c)) . -> . any)]
    (test-suite "reassociate-package-with-categories")
    
    ;;[associate-package-with-category (package? (or/c category? natural-number/c) . -> . void?)]
    (test-suite "associate-package-with-category") 
        
    ;;[get-package-categories (package? . -> . (listof category?))]
    (test-suite "get-package-categories"
      (test-equal? "1"
        (map category-name (get-package-categories (get-package "planet" "test-connection.plt")))
        (list "PLaneT-Related"))
      (test-equal? "2"
        (map category-id (get-package-categories (get-package-by-id 184 54)))
        (list 2 6 12)))
    
    ;;[pkgversion->primary-files (pkgversion? . -> . (listof primary-file?))]
    (test-suite "pkgversion->primary-files"
      (test-equal? "1" 
        (map primary-file-name (pkgversion->primary-files (get-package-version-by-id 80 18)))
        (list "test-connection.ss"))
      
      ;; need a multiple primary-file example
      )
    
    ;;[downloads-this-week (pkgversion? . -> . natural-number/c)]
    (test-suite "downloads-this-week")
    
    ;;[get-all-repositories (-> (listof repository?))]
    (test-suite "get-all-repositories"
      (test-equal? "1"
        (get-all-repositories)
        (list
         (make-repository 3
                          "4.x"
                          3990000  
                          4990000  
                          "4.x")
         (make-repository 2
                          "3xx"
                          2990000
                          3990000
                          "300"))))
    
    ;;[repository-ids->repositories (-> (listof natural-number/c) (listof repository?))]
    (test-suite "repository-ids->repositories"
      (test-equal? "1" (map repository-name (repository-ids->repositories '(2 3))) '("4.x" "3xx"))
      (test-equal? "2" (map repository-name (repository-ids->repositories '(0 1 2 3 4 5))) '("4.x" "3xx")))
    
    ;;[legal-repository? (-> number? boolean?)]
    (test-suite "legal-repository?"
      (test-equal? "1" (legal-repository? 1) #f)
      (test-equal? "2" (legal-repository? 2) #t)
      (test-equal? "3" (legal-repository? 3) #t)
      (test-equal? "4" (legal-repository? 4) #f))
    
    ;;[legal-language? (-> string? boolean?)]
    (test-suite "legal-language?"
      (test-equal? "1" (legal-language? "3.99.0.1") #t)
      (test-equal? "2" (legal-language? "4.0") #t)
      (test-equal? "3" (legal-language? "299.1") #t)
      (test-equal? "4" (legal-language? "372.3") #t)
      (test-equal? "5" (legal-language? "5.5") #f)
      (test-equal? "6" (legal-language? "12") #f)
      (test-equal? "7" (legal-language? "4.99.3") #f))
    
    ;;[add-pkgversion-to-db! (-> user? package? natural-number/c natural-number/c path? path? (-> symbol? (-> any) any)
    ;;                           natural-number/c)]
    (test-suite "add-pkgversion-to-db!")
    
    ;; [update-package-fields!
    ;;  (-> package? 
    ;;      pkgversion?
    ;;      (union (listof xexpr?) false/c) ;; package blurb
    ;;      (union string? false/c)         ;; package homepage
    ;;      (union (listof xexpr?) false/c) ;; package notes
    ;;    (union string? false/c) 
    ;;    (union string? false/c)
    ;;    void?)]
    (test-suite "update-package-fields!")
    
    ;;[update-pkgversion-fields!
    ;; (-> pkgversion?
    ;;     (union (listof xexpr?) false/c)
    ;;     (union string? false/c)
    ;;     (union string? false/c)
    ;;     void)]
    (test-suite "update-pkgversion-fields!")
    
    ;;[associate-pkgversion-with-repository! (natural-number/c (union repository? natural-number/c) . -> . void?)]
    (test-suite "associate-pkgversion-with-repository!")
    
    ;;[reassociate-pkgversion-with-repositories ((union pkgversion? natural-number/c) 
    ;;                                           (listof (union repository? natural-number/c))
    ;;                                           . -> .
    ;;                                           void?)]
    (test-suite "reassociate-pkgversion-with-repositories"
      (test-equal? "1"
        (let* ([all-reps (get-all-repositories)]
               [get-pv (λ () (package->current-version (get-package "planet" "test-connection.plt")))]
               [get-pv-reps (λ () (sort (pkgversion-repositories (get-pv)) <))]
               [pkgversion (get-pv)])
          (append
           (list
            (begin
              (reassociate-pkgversion-with-repositories pkgversion '())
              (get-pv-reps)))
           (map (λ (rep) 
                  (reassociate-pkgversion-with-repositories pkgversion (list rep))
                  (get-pv-reps))
                (sort (get-all-repositories) (λ (a b) (< (repository-id a) (repository-id b)))))
           (list
            (begin 
              (reassociate-pkgversion-with-repositories pkgversion all-reps)
              (get-pv-reps)))))
        '(() (2) (3) (2 3)))
      
      (test-equal? "2"
        (let* ([all-reps (get-all-repositories)]
               [get-pv (λ () (package->current-version (get-package "planet" "test-connection.plt")))]
               [get-pv-reps (λ () (sort (pkgversion-repositories (get-pv)) <))]
               [pkgversion (get-pv)])
          (append
           (list
            (begin
              (reassociate-pkgversion-with-repositories (pkgversion-id pkgversion) '())
              (get-pv-reps)))
           (map (λ (rep) 
                  (reassociate-pkgversion-with-repositories (pkgversion-id pkgversion) (list rep))
                  (get-pv-reps))
                (sort (get-all-repositories) (λ (a b) (< (repository-id a) (repository-id b)))))
           (list
            (begin 
              (reassociate-pkgversion-with-repositories (pkgversion-id pkgversion) all-reps)
              (get-pv-reps)))))
        '(() (2) (3) (2 3)))
      
      (test-equal? "3"
        (let* ([all-reps (get-all-repositories)]
               [get-pv (λ () (package->current-version (get-package "planet" "test-connection.plt")))]
               [get-pv-reps (λ () (sort (pkgversion-repositories (get-pv)) <))]
               [pkgversion (get-pv)])
          (append
           (list
            (begin
              (reassociate-pkgversion-with-repositories pkgversion '())
              (get-pv-reps)))
           (map (λ (rep) 
                  (reassociate-pkgversion-with-repositories pkgversion (list (repository-id rep)))
                  (get-pv-reps))
                (sort (get-all-repositories) (λ (a b) (< (repository-id a) (repository-id b)))))
           (list
            (begin 
              (reassociate-pkgversion-with-repositories pkgversion (map repository-id all-reps))
              (get-pv-reps)))))
        '(() (2) (3) (2 3)))
      
      (test-equal? "4"
        (let* ([all-reps (get-all-repositories)]
               [get-pv (λ () (package->current-version (get-package "planet" "test-connection.plt")))]
               [get-pv-reps (λ () (sort (pkgversion-repositories (get-pv)) <))]
               [pkgversion (get-pv)])
          (append
           (list
            (begin
              (reassociate-pkgversion-with-repositories (pkgversion-id pkgversion) '())
              (get-pv-reps)))
           (map (λ (rep) 
                  (reassociate-pkgversion-with-repositories (pkgversion-id pkgversion) (list (repository-id rep)))
                  (get-pv-reps))
                (sort (get-all-repositories) (λ (a b) (< (repository-id a) (repository-id b)))))
           (list
            (begin 
              (reassociate-pkgversion-with-repositories (pkgversion-id pkgversion) (map repository-id all-reps))
              (get-pv-reps)))))
        '(() (2) (3) (2 3)))
      )
    
    ;;[get-next-version-number (-> package? boolean? (cons/c natural-number/c natural-number/c))]
    (test-suite "get-next-version-number"
      (test-equal? "1" 
        (get-next-version-number (get-package "planet" "test-connection.plt") #f)
        (cons 2 0))
      (test-equal? "2" 
        (get-next-version-number (get-package "planet" "test-connection.plt") #t)
        (cons 1 1))
      (test-equal? "3"
        (get-next-version-number (get-package "jacobm" "crypto.plt") #f)
        (cons 2 0))
      (test-equal? "4"
        (get-next-version-number (get-package "jacobm" "crypto.plt") #t)
        (cons 1 15))
      (test-equal? "5"
        (get-next-version-number (get-package "jacobm" "resume.plt") #f)
        (cons 4 0))
      (test-equal? "6"
        (get-next-version-number (get-package "jacobm" "resume.plt") #t)
        (cons 3 1)))
    
    ;;[get-next-version-for-maj (-> package? natural-number/c natural-number/c)]
    (test-suite "get-next-version-for-maj"
      (test-equal? "1"
        (get-next-version-for-maj (get-package "robby" "redex.plt") 1)
        4)
      (test-equal? "2"
        (get-next-version-for-maj (get-package "robby" "redex.plt") 2)
        7)
      (test-equal? "3"
        (get-next-version-for-maj (get-package "robby" "redex.plt") 3)
        7)
      (test-exn "4"
                (λ (e) 
                  (and (exn:fail? e)
                       (string=? (exn-message e)
                                 "get-next-version-for-maj: specified major version does not exist")))
                (λ () (get-next-version-for-maj (get-package "robby" "redex.plt") 4))))
    
    ;;[log-download
    ;; (string?  ; ip address
    ;;  pkgversion? ; the downloaded package
    ;;  string?  ; client core version
    ;;  . -> . void?)]
    (test-suite "log-download")
    
    ;;[log-error
    ;;(string?          ; ip address
    ;; string?          ; freeform error message
    ;; . -> . void?)]
    (test-suite "log-error")
    
    ;;[for-each-package-version ((package? pkgversion? . -> . any) . -> . any)]
    (test-suite "for-each-package-version")
    
    ;;[core-version-string->code (string? . -> . (union number? false/c))]
    (test-suite "core-version-string->code"
      (test-equal?  "1" (core-version-string->code "300")        3000000)
      (test-equal?  "2" (core-version-string->code "369.2")      3690002)
      (test-equal?  "3" (core-version-string->code "369.20")     3690020)
      (test-equal?  "4" (core-version-string->code "3.99")       3990000)
      (test-equal?  "5" (core-version-string->code "3.99.0.2")   3990002)
      (test-equal?  "6" (core-version-string->code "3.99.0.23")  3990023)
      (test-equal?  "7" (core-version-string->code "3.99.48.23") 3994823)
      (test-equal?  "8" (core-version-string->code "4.9.0")      4090000)
      (test-equal?  "9" (core-version-string->code "4.9.2")      4090200)
      (test-equal? "10" (core-version-string->code "5.10")       5100000))
    
    ;;[code->core-version-string (number? . -> . (union string? false/c))]
    (test-suite "code->core-version-string"
      (test-equal?  "1" (code->core-version-string 3000000) "300")
      (test-equal?  "2" (code->core-version-string 3690002) "369.2")
      (test-equal?  "3" (code->core-version-string 3690020) "369.20")
      (test-equal?  "4" (code->core-version-string 3990000) "3.99")       
      (test-equal?  "5" (code->core-version-string 3990002) "3.99.0.2")
      (test-equal?  "6" (code->core-version-string 3990023) "3.99.0.23")
      (test-equal?  "7" (code->core-version-string 3994823) "3.99.48.23")
      (test-equal?  "8" (code->core-version-string 4090000) "4.9")
      (test-equal?  "9" (code->core-version-string 4090200) "4.9.2")
      (test-equal? "10" (code->core-version-string 5100000) "5.10"))
    
    ;; [recompute-all-primary-files (-> any)]
    (test-suite "recompute-all-primary-files")
    
    ;;[get-n-most-recent-packages (natural-number/c (union natural-number/c repository?) . -> . (listof package?))]
    (test-suite "get-n-most-recent-packages")
    
    ;;[blurb->xexprs (any/c . -> . (union (listof xexpr?) false/c))]
    (test-suite "blurb->xexprs"
      (test-equal? "1" (andmap xexpr? (blurb->xexprs "hello")) #t) 
      (test-equal? "2" (andmap xexpr? (blurb->xexprs '((p "hello") (div "this is a " (p "test"))))) #t)
      (test-equal? "3" (blurb->xexprs 1242342) #f))
    
    (test-suite "groupby"
      (test-equal? "1" 
        (sort (groupby even? (build-list 10 values)) (λ (a b) (< (car a) (car b))))
        '((0 2 4 6 8)
          (1 3 5 7 9)))
      (test-equal? "2" 
        (sort (groupby (λ (x) (modulo x 3)) (build-list 10 values)) (λ (a b) (< (car a) (car b))))
        '((0 3 6 9)
          (1 4 7)
          (2 5 8))))))

(define-check (check-not-exn/stack thunk)
  (with-handlers ((exn:fail? (λ (e)
                               (let* ([strp (open-output-string)])
                                 (parameterize ([current-error-port strp])
                                   ((error-display-handler) (exn-message e) e))
                                 (with-check-info (['stack (get-output-string strp)])
                                   (raise e))))))
    (thunk)))


(define dummy-req
  (make-request 'GET 
                (string->url "http://planet.plt-scheme.org")
                '()
                '()
                #f
                "127.0.0.1"
                80
                "127.0.0.1"))

(define display-tests
  (test-suite "display.ss tests"
    (test-suite "gen-package-page"
      (test-case "all packages generate pages"
        (check-not-exn/stack
         (λ ()
           (let ([all-packages (append-map user->packages (get-all-users))])
             (parameterize ([req dummy-req]      
                            [rep-id 3]
                            [rep-explicit? #f]
                            [rep (car (get-all-repositories))])
               (for-each
                (λ (u) (with-check-info (['usr u])
                         (for-each
                          (λ (pkg)
                            (with-check-info (['pkg pkg])
                              (gen-package-page pkg)))
                          (with-check-info (['user u])
                            (user->packages u)))))
                (get-all-users))))))))))
          

(define server-tests
  (test-suite
      "server integration"
    (test-suite "server provides correct package for require spec"
      (test-case "1"
        (check-server-gives-file '("planet" "test-connection.plt") 1 0))
      (test-case "2"
        (check-server-gives-file '("planet" "test-connection.plt" 1) 1 0))
      (test-case "3"
        (check-server-gives-file '("planet" "test-connection.plt" 1 0) 1 0))
      (test-case "4"
        (check-server-gives-file '("planet" "test-connection.plt" 1 (- 7)) 1 0))
      (test-case "5"
        (check-server-gives-file '("planet" "test-connection.plt" 1 (+ 0)) 1 0))
      (test-case "6"
        (check-server-gives-file '("planet" "test-connection.plt" 1 (0 2)) 1 0))
      (test-case "7"
        (check-server-gives-file '("planet" "test-connection.plt" 1 (= 0)) 1 0))
      (test-case "8"
        (check-server-has-no-match '("planet" "test-connection.plt" 1 1)))
      (test-case "9"
        (check-server-has-no-match '("planet" "test-connection.plt" 1 (+ 1))))
      (test-case "10"
        (check-server-has-no-match '("planet" "test-connection.plt" 1 (1 5))))
      (test-case "11"
        (check-server-has-no-match '("planet" "test-connection.plt" 1 (= 1))))
      (test-case "12"
        (check-server-has-no-match '("planet" "test-connection.plt" 2))))))

(define all-tests
  (test-suite "all"
    '#:before (λ () 
                (HTTP-DOWNLOAD-SERVLET-URL "http://localhost:8080/servlets/planet-servlet.ss")
                (initialize-testing-database!))
    '#:after (λ ()
               (HTTP-DOWNLOAD-SERVLET-URL real-url))
    db.ss-tests
    display-tests
    server-tests))

(test/text-ui all-tests)
             
   