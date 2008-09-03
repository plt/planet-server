(module demands mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "contract.ss")
           (lib "match.ss")
           (lib "etc.ss")
           "html.ss"
           (prefix srfi1: (lib "1.ss" "srfi")))
  
  
  (define (kv-pair? p)
    (and (pair? p)
         (symbol? (car p))
         (or (string? (cdr p))
             (bytes? (cdr p)))))
  
  (define (problem? v) 
    (match v
      [`(,(? symbol?) (message ,@(_ ...))) #t]
      [`(general ,@(_ ...)) #t]
      [_ #f]))
  
  (define demand/c (-> (listof kv-pair?) (listof problem?)))
  
  (provide/contract 
   [all-demands (-> (listof demand/c) demand/c)]
   [or-demand   (-> (cons/c demand/c (listof demand/c)) demand/c)]
   [field-exists (-> symbol? demand/c)]
   [field-absent (-> symbol? demand/c)]
   [field-in     (-> symbol? (listof symbol?) demand/c)]
   [field-nonblank (-> symbol? demand/c)]
   [field-numeric  (-> symbol? demand/c)]
   [field-in-range (-> symbol? natural-number/c natural-number/c demand/c)]
   [fields-exist (-> (listof symbol?) demand/c)]
   [fields-nonblank (-> (listof symbol?) demand/c)]
   [fields-ascii (->* () (listof symbol?) (demand/c))]
   [field-constraint ((procedure? #|should take the number of strings = to the given # of symbols, at least, and return problems|#) 
                      (listof symbol?)
                      . ->* .
                      (demand/c))]
   [field-lengths<= (->* (natural-number/c) (listof symbol?) (demand/c))]
   [field-lengths-in (->* (natural-number/c natural-number/c) (listof symbol?) (demand/c))]
   [field-lengths>= (->* (natural-number/c) (listof symbol?) (demand/c))]
   ;; surely this next one is a bad api
   [wrap-as-demand-p (;(->* () (listof string?) any) (->* () (listof string?) ((listof problem?))) ;; i want an -> that says "i'm not enforcing arity"
                      procedure? procedure?
                      . -> .
                      (->* () (listof string?) ((listof problem?))))]
   [problem? (-> any/c boolean?)]
   [demand/c contract?])
  
  ;; ============================================================
  ;; form validation / demand combinators
  
  ;; the following are demand combinators
  
  (define (all-demands demands)
    (lambda (b) (apply append (map (λ (d) (d b)) demands))))
  
  (define (or-demand demands)
    (lambda (b)
      (let ([problems (map (λ (d) (d b)) demands)])
        (cond
          [(ormap null? problems) '()]
          [else (car (reverse problems))]))))
  
  (define (field-exists f)
    (lambda (b)
      (if (exists-binding? f b)
          '()
          `((,f `(message "required"))))))
  
  (define (field-absent f)
    (lambda (b)
      (if (exists-binding? f b)
          `((general (format "field ~a must be absent" f)))
          '())))
  
  (define (field-in f options)
    (lambda (b)
      (cond
        [(not (exists-binding? f b))
         `((,f (message "required")))]
        [(not (memq (string->symbol (car (extract-bindings f b))) options))
         `((,f (message "not a legal choice")))]
        [else '()])))
  
  (define (field-nonblank f)
    (lambda (b)
      (cond
        [(and (exists-binding? f b)
              (not (regexp-match #rx"^[ \t\n]*$" (car (extract-bindings f b)))))
         '()]
        [else
         `((,f (message "required")))])))
  
  (define (field-numeric f)
    (lambda (b)
      (cond
        [(and (exists-binding? f b)
              (regexp-match #rx"^[0-9]+$" (car (extract-bindings f b))))
         '()]
        [else `((,f (message "must be numeric")))])))
  
  (define (field-in-range f lo hi)
    (lambda (b)
      (let ([others ((field-numeric f) b)])
        (cond
          [(null? others)
           (if (<= lo (string->number (car (extract-bindings f b))) hi)
               '()
               `((,f (message ,(format "must be in range [~a,~a]" lo hi)))))]
          [else others]))))
  
  
  (define (fields-exist fields)
    (all-demands (map-i field-exists fields)))
  
  (define (fields-nonblank fields)
    (all-demands (map-i field-nonblank fields)))
  
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
        (let ([req (send/suspend/doctype (problems->k->html problems))])
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
  
  ;; ============================================================
  ;; utility
  
  (define (map-i f args)
    (apply list-immutable (map f args)))
  
  
  )