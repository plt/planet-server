(module demands mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "contract.ss")
           (lib "match.ss")
           (lib "xml.ss" "xml")
           (lib "etc.ss")
           (prefix srfi1: (lib "1.ss" "srfi")))
  
  (provide 
   all-demands
   field-exists
   field-nonblank
   fields-exist
   fields-nonblank
   fields-ascii
   field-constraint
   field-lengths<=
   field-lengths-in
   field-lengths>=
   wrap-as-demand-p
   problem?)
  
  (define (problem? v) 
    (match v
      [`(,(? symbol?) (message ,@(_ ...))) #t]
      [`(general ,@(_ ...)) #t]
      [_ #f]))
  
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
  
  
  (define (fields-constraint pred gen-message)
    (λ fields
      (λ (b)
        (srfi1:filter-map
         (λ (field)
           (if (or (not (exists-binding? field b))
                   (pred (car (extract-bindings field b))))
               #f
               `(,field (message ,@(gen-message (extract-bindings field b))))))
         fields))))
  
  (define (field-lengths<= n . fields)
    (apply (fields-constraint
            (λ (b) (<= (string-length b) n))
            (λ (b) `("Must be <= " ,(number->string n) " letters")))
           fields))
  
  (define (field-lengths-in min max . fields)
    (apply (fields-constraint
            (λ (b) (and (>= (string-length b) min)
                        (<= (string-length b) max)))
            (λ (b) `("Must be from " ,(number->string min) " to " ,(number->string max) " letters")))
           fields))
  
  (define (field-lengths>= n . fields)
    (apply (fields-constraint 
            (λ (b) (>= (string-length b) n))
            (λ (_) `("Must be >= " ,(number->string n) " letters")))
           fields))
  
  (define fields-ascii
    (fields-constraint
     (λ (b) 
       (with-handlers ([exn:fail:contract? (λ (e) #f)])
         (begin 
           (string->bytes/latin-1 b)
           #t)))
     (λ (b) `("Must consist entirely of ASCII characters"))))
  
  (define (wrap-as-demand-p pred formatter)
    (λ args
      (let ([ans (apply pred args)])
        (if ans
            '()
            (list (apply formatter args))))))
    
  ;; ============================================================
  ;; higher-level interface via send/suspend/demand
  (provide (struct demand-page (>problems->k->html >demands)))
  (define-struct demand-page (>problems->k->html >demands))
  
  (provide/contract
   [send/suspend/demand
    (demand-page? . -> . request?)])
  
  (define (send/suspend/demand demand-page)
    (let ([problems->k->html (demand-page->problems->k->html demand-page)]
          [demands (demand-page->demands demand-page)])
      (let loop ([problems '()])
        (let ([req (send/suspend (problems->k->html problems))])
          (let ([new-problems (demands (request-bindings req))])
            (if (null? new-problems)
                req
                (loop new-problems)))))))
  
  (provide/contract
   [with-problems
    (((listof problem?) 
      (->
       (listof string?)
       (opt-> (symbol?) ((any/c . -> . any)) any)
       (opt-> (symbol?) ((any/c . -> . any)) any)
       any))
     ((-> any/c any)
      (-> any/c any))
     . opt-> . any)])
  
  (define (extract key items)
    (srfi1:filter-map (lambda (pr) (and (eq? (car pr) key) (cadr pr))) items))
  
  (define with-problems
    (opt-lambda (problems 
                 finisher
                 [value-formatter (λ (x) `(value ,@x))]
                 [message-formatter (λ (x) `(small ((class "errorMsg")) ,@x))]) 
      (let* ([general-error-messages (extract 'general problems)]
             [extractor
              (lambda (field default-wrapper)
                (opt-lambda (key [wrapper default-wrapper])
                  (let ([pr (assq key problems)])
                    (if pr
                        (let ([pr2 (assq field (cdr pr))])
                          (if pr2
                              (list (wrapper (cdr pr2)))
                              '()))
                        '()))))]
             [value-for (extractor 'value value-formatter)]
             [message-for (extractor 'message message-formatter)])
        (finisher general-error-messages value-for message-for))))
  
  
  )