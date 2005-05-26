(module repository-tests mzscheme
  
  (require "repository.ss"
           (lib "file.ss"))
  
  (define tests-run 0)
  (define tests-passed 0) 
  
  (define-syntax (test-group stx)
    (syntax-case stx ()
      [(_ name e ...)
       #`(begin
           (printf "Running test-group ~a ...\n" 'name)
           e ...
           (printf "~a out of ~a tests passed\n" tests-passed tests-run)
           (set! tests-run 0)
           (set! tests-passed 0))]))
       
  (define-syntax (test stx)
    (syntax-case stx ()
      [(_ p test expected)
       #'(let ((ans test)
               (e expected))
           (if (p ans e)
               (set! tests-passed (add1 tests-passed))
               (printf "TEST FAILED:\n  ~s\n  expected ~s\n  received ~s\n" 'test e ans))
           (set! tests-run (add1 tests-run)))]))
  
  ;; dtree ::= (symbol dtree ...)
  (define (make-directory-tree root dtree)
    (make-directory* (build-path root (format "~a" (car dtree))))
    (for-each
     (lambda (x) (make-directory-tree (build-path root (format "~a" (car dtree))) x))
     (cdr dtree)))
  
  (define (remove-directory-tree root dtree)
    (let ((subtrees (map (lambda (d) (remove-directory-tree (build-path root (format "~a" (car dtree))) d)) (cdr dtree))))
      (if (andmap (lambda (x) x) subtrees)
          (with-handlers ([exn:fail:filesystem? (lambda (e) #f)])
            (delete-directory (build-path root (format "~a" (car dtree))))
            #t)
          #f)))
      
  
  
  (test-group 
   "Repository/Package tests"
   
   (define p (make-package-descriptor "add-blaster.plt" '() "jacob" 1 0))
   (test equal? (key p) '("jacob" . "add-blaster.plt"))
   
   (define ht (make-hash-table 'equal))
   (add-to-cache ht p)
   (test equal? (hash-table-map ht list)
         `((("jacob" . "add-blaster.plt") (,p))))
   
   (test = (extract-max '(1 2 5 4 3 2 1) <) 5)
   (test = (extract-max '(1 2 5 4 3 2 1) >) 1)
   
   
   (define test-dir
     '(test-repository (jacob (add-blaster (1 (0) (1) (2)))
                              (foo (1 (0)) (2 (0))))
                       (fred (boo-blaster (1 (0))))))
   
   ;(make-directory-tree "/tmp" test-dir)
   ;(build-cache (build-path "/tmp" "test-repository"))
   ;(define c (cache-
   
   
   ))