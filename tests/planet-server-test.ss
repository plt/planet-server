(module planet-server-test mzscheme
  
  (require "../planet-server.ss"
           "../configuration.ss"
           (lib "planet-shared.ss" "planet" "private")
           (lib "match.ss")
           (lib "stxparam.ss"))
           
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 5)))
  (require-for-syntax (lib "list.ss"))
  
  (provide (all-defined))
  
  (define-syntax-parameter current-tester (syntax-rules () [(current-tester) n]))
  (define-syntax-parameter current-file* (syntax-rules () [(current-file*) file*]))
  
  (define (natural-number? n) (and (integer? n) (>= n 0)))
  
  (define (instrumented-call lv spec)
    (parameterize ([DATABASE-CONNECT-ARGUMENTS '("localhost" 5432 "planet_test" "tester" "testee")])
      (let ([tfiles '()]
            [tfails '()]
            [proceeds 0]
            [stops 0])
        (handle-one-request lv spec 
                            (λ (_ f) (set! tfiles (append tfiles (list f))))
                            (λ (spec sym str) (set! tfails (append tfails (list (list sym str)))))
                            (λ () (set! proceeds (add1 proceeds)))
                            (λ () (set! stops (add1 stops))))
        (list tfiles tfails proceeds stops))))
  
  (define (req->pkg-spec owner pkg maj min requester)
    (let-values ([(min-lo min-hi)
                  (match min
                    [(? natural-number?)
                     (values min #f)]
                    [`(- ,(? natural-number? n))
                      (values 0 n)]
                    [`(+ ,(? natural-number? n))
                      (values n #f)]
                    [`(= ,(? natural-number? n))
                      (values n n)]
                    [#f
                     (values 0 #f)]
                    [_ (error 'req->pkg-spec "invalid minor version specifier")])])
      (make-pkg-spec pkg maj min-lo min-hi (list owner) #f requester)))
  
  (define (n* owner pkg maj min ver)
    (instrumented-call ver (req->pkg-spec owner pkg maj min ver)))
  (define (n owner pkg maj min)
    (n* owner pkg maj min "369.8"))
  
  (define (file owner name maj min)
    (build-path (FILE-STORAGE-PATH) owner name (number->string maj) (number->string min) name))
  
  (define (old-file owner name maj min)
    (build-path "/" "local" "planet" "20x" owner name (number->string maj) (number->string min) name))
  
  (define (file* owner name maj min)
    (list (list (file owner name maj min)) '() 1 0))
  
  (define (old-file* owner name maj min)
    (list (list (old-file owner name maj min)) '() 1 0))
  
  (define (normal-error* sym message)
    (list '() `((,sym ,message)) 1 0))

  (define-syntax (package-test-suite* stx)
    
    (define ((numbers->tests ownerV nameV) majV highest-minV)
      (with-syntax ([owner ownerV]
                    [name nameV]
                    [maj majV]
                    [highest-min highest-minV])
        (let loop ([ictr 0])
          (with-syntax ([i ictr])
            (cond
              [(> ictr highest-minV) 
               (list #`(test-equal?
                        #,(format "(~a ~a ~a ~a) [nonexistant package]"
                                  (syntax-object->datum ownerV)
                                  (syntax-object->datum nameV)
                                  majV
                                  ictr)
                        ((current-tester) owner name maj i)
                        (normal-error* 'not-found "No package matched the specified criteria"))
                     #`(test-equal?
                        #,(format "(~a ~a ~a #f)"
                                  (syntax-object->datum ownerV)
                                  (syntax-object->datum nameV)
                                  majV)
                        ((current-tester) owner name maj #f)
                        ((current-file*)  owner name maj highest-min)))]
              [else
               (list* #`(test-equal? 
                         #,(format "(~a ~a ~a ~a)"
                                   (syntax-object->datum ownerV)
                                   (syntax-object->datum nameV)
                                   majV
                                   ictr)
                         ((current-tester) owner name maj i)
                         ((current-file*)  owner name maj highest-min))
                      #`(test-equal?
                         #,(format "(~a ~a ~a (= ~a))"
                                   (syntax-object->datum ownerV)
                                   (syntax-object->datum nameV)
                                   majV
                                   ictr)
                         ((current-tester) owner name maj `(= ,i))
                         ((current-file*)  owner name maj i))
                      #`(test-equal?
                         #,(format "(~a ~a ~a (- ~a))"
                                   (syntax-object->datum ownerV)
                                   (syntax-object->datum nameV)
                                   majV
                                   ictr)
                         ((current-tester) owner name maj `(- ,i))
                         ((current-file*)  owner name maj i))
                      (loop (add1 ictr)))])))))

    (syntax-case stx ()
      [(_ owner name (maj highest-min) ...)
       (let* ([very-highest (car (last-pair (syntax-object->datum #'((maj highest-min) ...))))]
              [very-highest-maj (car very-highest)]
              [very-highest-min (cadr very-highest)])
         #`(test-suite 
            #,(format "~a/~a tests" (syntax-object->datum #'owner) (syntax-object->datum #'name))
            #,@(apply append 
                      (map (numbers->tests #'owner #'name)
                           (syntax-object->datum #'(maj ...))
                           (syntax-object->datum #'(highest-min ...))))
            (test-equal? #,(format "(~a ~a #f #f)" (syntax-object->datum #'owner) (syntax-object->datum #'name))
                         ((current-tester) owner name #f #f)
                         ((current-file*) owner name #,very-highest-maj #,very-highest-min))))]))
  
  (define-syntax (package-test-suite stx)
    (syntax-case stx ()
      [(_ o p (maj min) ...)
       #'(package-test-suite* o p (maj min) ...)]
      [(_ o p v (maj min) ...)
       (let ([version (syntax-object->datum #'v)])
         #`(let ([run-test (λ (owner name mj mn) (n* owner name mj mn v))])
             (syntax-parameterize ([current-tester (syntax-rules () [(current-tester) run-test])]
                                   #,@(if (regexp-match #rx"^2" version)
                                         (list #`[current-file* (syntax-rules () [(current-file*) old-file*])])
                                         '()))
                (package-test-suite* o p (maj min) ...))))]))
  
  (define manual-tests
    (test-suite
     "manually-crafted tests"
     (test-equal? "basic 1"
                  (n "planet" "test-connection.plt" 1 0) 
                  (file* "planet" "test-connection.plt" 1 0))
     (test-equal? "basic 2" 
                  (n "planet" "test-connection.plt" 1 #f) 
                  (file* "planet" "test-connection.plt" 1 0))
     (test-equal? "basic 3" 
                  (n "planet" "test-connection.plt" #f #f) 
                  (file* "planet" "test-connection.plt" 1 0))
     
     (test-equal? "required core version test 1"
                  (n* "robby" "redex.plt" #f #f "300.5")
                  (file* "robby" "redex.plt" 2 0))
     (test-equal? "required core version test 2"
                  (n* "robby" "redex.plt" #f #f "307.9")
                  (file* "robby" "redex.plt" 3 5))
     (test-equal? "required core version test 3"
                  (n* "robby" "redex.plt" 3 #f "300.9")
                  (normal-error* 'not-found "Your version of PLT Scheme is too old to run the specified package"))))
  
  (define automatic-tests
    (test-suite
     "exhaustive tests for particular packages"
     (package-test-suite "planet" "test-connection.plt" 
                         (1 0))
     (package-test-suite "dherman" "javascript.plt" 
                         (1 0)
                         (2 3)
                         (3 5)
                         (4 1)
                         (5 2))
     (package-test-suite "robby" "redex.plt"
                         (1 3)
                         (2 6)
                         (3 6))
     
     ))
  
  (define automatic-20x-tests
    (test-suite
     "backwards-compatibility tests for 20x packages"
     (package-test-suite "planet" "test-connection.plt" "207.1"
                         (1 1))
     (package-test-suite "soegaard" "galore.plt" "207.1"
                         (1 1))
     (package-test-suite "dherman" "io.plt" "207.1"
                         (1 1))
     (package-test-suite "dherman" "zip.plt" "207.1"
                         (1 2))))
  
  (define all-tests
    (test-suite "all tests"
                manual-tests
                automatic-tests
                automatic-20x-tests))
  )