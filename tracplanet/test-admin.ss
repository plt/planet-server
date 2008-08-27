#lang scheme/base
(require 
         "trac-admin.ss"
         "xmlrpc/xml-rpc.ss"
         (lib "scheme/system")
         (lib "scheme/file")
         (lib "list.ss")
         (planet "digest.ss" ("soegaard" "digest.plt" 1 2))
         (planet "test.ss" ("dherman" "test.plt" 1 3)))


(define path-to-env "/local/bugs/tracfiles")
(define env-path (string->path path-to-env))





;void->bool
;checks to see if the basic admin functions work properly.
(define (test-trac-admin)
    (let* ([return-values (list (test-component-list)
                                (test-component-add) ;also tests list-component;
                                (test-component-remove)
                                (test-ticket-remove)
                                (test-permission-add-and-remove)
                                (test-user-add)
                                (test-user-remove)
                                (test-change-password)
                              
                                )]
           [new-values (andmap (lambda(x)  x) return-values)])
      (begin
        (when (user-exists? "jt")
          (user-remove "jt"))
        new-values)))
  


;void->boolean?
(define (test-component-add)
  (let* ([live-port (add-component "testcomponent" "jamiemmt")]
         [listed (list-component)]
         [created? (regexp-match "testcomponent" listed)])
    (and (close-input-port live-port)
         (close-input-port listed)
         (if (not created?) #f
             #t))))


(define (test-permission-add-and-remove)
  (begin
  (permission-add "foo" "TICKET_CREATE")
  (let* ([permissions (permission-list)]
         [created? (member "foo            TICKET_CREATE  "  permissions)]
         [false? (boolean? created?)])
    (begin
      (permission-remove "foo" "TICKET_CREATE")
      (let* ([restored-permissions (permission-list)]
             [removed? (boolean? (member "foo            TICKET_CREATE  " restored-permissions))])
      (and removed? (not false?)))))))
  
;void->boolean?
(define (test-component-list)
  (let* ([live-port (list-component)]
         [working? (regexp-match "benchmark.plt" live-port)])
    (and (close-input-port live-port)
         (if (not working?) #f
             #t))))
         
;void->boolean?
(define (test-component-remove)
  (and (remove-component "testcomponent")
       (let* ([list (list-component)]
              [removed? (not (regexp-match "testcomponent" list))])
         removed?)))

(define (test-ticket-remove)
  (let ([tick-id (ticket-create-stdin "should be removed" "should be removed" '())])
    (begin (ticket-remove tick-id)
           (empty? (ticket-get tick-id)))))

(define (test-user-add)
  (let* ([user     (user-add "jt" "tj")]
         [proper? (string=? "jt:trac:5eb55b1cf644fa83bd416c15c47cdd06" user)])
    (and proper? (user-exists? "jt") )))

(define (test-user-remove)
  (begin 
    (user-remove "jt")
    (not (user-exists? "jt"))))


(define (test-change-password)
  (begin0 (user-add "jt" "pass")
          (user-change-password "jt" "pass"))
  (let ([pfile (open-input-file "/local/password/users.txt")])
          (and (user-exists? "jt")
               (let loop()
                 (let* ([line (read-line pfile)])
                   (eof-object? line)
                   #f
                   (if (regexp-match (regexp "jt") line)
                       (if (boolean? (regexp-match (regexp "55ef5b5136df4bb5afcda9f5527a1387") line))
                           #f
                           #t)
                       (loop)))))))