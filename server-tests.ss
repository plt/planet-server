#lang scheme/base

(require (planet schematics/schemeunit:2:10/test)
         
         scheme/system
         planet/util
         planet/config)

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

(define server-tests
  (test-suite
   "Tests for the planet server"
   '#:before (λ ()
               (HTTP-DOWNLOAD-SERVLET-URL "http://localhost:8080/servlets/planet-servlet.ss")
               (initialize-testing-database!))
   '#:after (λ ()
               (HTTP-DOWNLOAD-SERVLET-URL real-url))
   
   (test-suite "Server provides correct package for require spec"
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
   