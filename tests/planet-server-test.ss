(module planet-server-test mzscheme
  
  (require "../planet-server.ss"
           "../configuration.ss"
           (lib "planet-shared.ss" "planet" "private")
           (lib "match.ss"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 5)))

  (define (natural-number? n) (and (integer? n) (>= n 0)))
  
  (define (instrumented-call lv spec)
    (let ([tfiles '()]
          [tfails '()]
          [proceeds 0]
          [stops 0])
      (handle-one-request lv spec 
                          (λ (_ f) (set! tfiles (append tfiles (list f))))
                          (λ (spec sym str) (set! tfails (append tfails (list (list spec sym str)))))
                          (λ () (set! proceeds (add1 proceeds)))
                          (λ () (set! stops (add1 stops))))
      (list tfiles tfails proceeds stops)))
  
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
  
  (define (file owner name maj min)
    (build-path (FILE-STORAGE-PATH) name (number->string maj) (number->string min) name))
  
  (define (file* owner name maj min)
    (list (list (file owner name maj min)) '() 1 0))
  
  (define success-tests
    (let ([n (λ (owner pkg maj min)
               (instrumented-call "369.8" (req->pkg-spec owner pkg maj min "369.8")))])
      (test-suite
       "handle-one-request success tests"
       (test-equal? "1" 
                    (n "planet" "test-connection.plt" 1 0) 
                    (file* "planet" "test-connection.plt" 1 0)))))
  
  )