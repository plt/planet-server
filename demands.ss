(module demands mzscheme
  (require (lib "servlet.ss" "web-server")
           (prefix srfi1: (lib "1.ss" "srfi")))
  
  (provide (all-defined))
  
  ;; ============================================================
  ;; form validation / demand combinators
  
  ;; demand ::= (bindings -> (listof problem)) 
  ;; the following are demand combinators
  
  (define (all-demands . demands)
    (lambda (b) (apply append (map (λ (d) (d b)) demands))))
  
  (define (field-exists f)
    (lambda (b)
      (if (exists-binding? f b)
          '()
          `((,f `(message "required"))))))
  
  (define (field-nonblank f)
    (lambda (b)
      (cond
        [(and (exists-binding? f b)
              (not (regexp-match #rx"^[ \t\n]*$" (car (extract-bindings f b)))))
         '()]
        [else
         `((,f (message "required")))])))
  
  
  (define (fields-exist fields)
    (apply all-demands (map field-exists fields)))
  
  (define (fields-nonblank fields)
    (apply all-demands (map field-nonblank fields)))
  
  (define (field-constraint okay? . fields)
    (lambda (b)
      (cond
        [(not (andmap (λ (f) (exists-binding? f b)) fields))
         '()]
        [(apply okay? (map (λ (f) (car (extract-bindings f b))) fields))
         =>
         (λ (result) (or result '()))])))
  
  (define (field-lengths<= n . fields)
    (λ (b)
      (srfi1:filter-map
       (λ (field)
         (if (or (not (exists-binding? field b))
                 (<= (string-length (car (extract-bindings field b))) n))
             #f
             `(,field (message "Too long. Please use " ,(number->string n) " letters or fewer"))))
       fields)))
  
  (define (wrap-as-demand-p pred formatter)
    (λ args
      (let ([ans (apply pred args)])
        (if ans
            '()
            (list (apply formatter args))))))
  
  )
