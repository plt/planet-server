(module html2text mzscheme
  
  (require (lib "match.ss")
           (lib "xml.ss" "xml")
           (lib "contract.ss"))

  (provide/contract 
   (html-expr->text (xexpr? . -> . string?)))
  
  (define (xexpr->xexpr e)
    (parameterize ((xexpr-drop-empty-attributes #f))
      (xml->xexpr (xexpr->xml e))))
  
  ;; html-expr->text : xexpr[html] -> string
  ;; converts the given html expr into a semi-rendered string version
  ;; this isn't really very good, but it's probably good enough for
  ;; my purposes
  (define (html-expr->text expr)
    (define (html-expr->pre-list expr)
      (match expr
        [(? string? expr) (list expr)]
        [`(p ,(_ ...) ,@(subexprs ...)) `(newline ,@(apply append (map html-expr->pre-list subexprs)) newline)]
        [`(br ,(_ ...)) (list 'newline)]
        [(? symbol?) (list (symbol->string expr))]
        [`(,(? symbol?) ,(_ ...) ,@(subexprs ...)) (apply append (map html-expr->pre-list subexprs))]))
    
    (define (whitespace? c)
      (cond
        [(char? c) (char-whitespace? c)]
        [(eq? c 'whitespace) #t]))
    
    (define (nonwhite-finish? c)
      (eq? c 'nonwhite-finish))
    
    (list->string
     (let loop ((strs (html-expr->pre-list (xexpr->xexpr expr)))
                (last-char 'newline))
       (cond
         [(null? strs) '()]
         [(eq? (car strs) 'newline) (cons #\newline (loop (cdr strs) 'whitespace))]
         [else
          (let str-loop ((chars (string->list (car strs)))
                         (last-char last-char))
            (cond
              [(null? chars)
               (loop (cdr strs) (if (whitespace? last-char) last-char 'nonwhite-finish))]
              [(char-whitespace? (car chars))
               (if (whitespace? last-char)
                   (str-loop (cdr chars) last-char)
                   (cons #\space (str-loop (cdr chars) 'whitespace)))]
              [else
               (if (nonwhite-finish? last-char)
                   (cons #\space (cons (car chars) (str-loop (cdr chars) (car chars))))
                   (cons (car chars) (str-loop (cdr chars) (car chars))))]))])))))