#lang scheme/base

(require (planet schematics/schemeunit:2:10/test)
         (planet schematics/schemeunit:2:10/text-ui)
         
         scheme/system
         planet/util
         planet/config
         
         "db.ss"
         "data-structures.ss")

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
    
    (test-suite "repository util"
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
                                     "300")))
      (test-equal? "2" (legal-repository? 1) #f)
      (test-equal? "3" (legal-repository? 2) #t)
      (test-equal? "4" (legal-repository? 3) #t)
      (test-equal? "5" (legal-repository? 4) #f))
    
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
                                             '())))))

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
   server-tests))

(test/text-ui all-tests)
             
   