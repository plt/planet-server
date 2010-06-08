#lang scheme/base
(require net/cookie
         web-server/servlet/bindings
         web-server/private/request-structs
         web-server/private/response-structs
         xml/xml
         scheme/contract)

(define doctype
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")

(define-struct client-cookie 
  (name
   value
   [domain #:mutable]
   [path #:mutable]) 
  #:transparent)

(provide build-cookie
         request-cookies
         (struct-out client-cookie))

(define (not-response? x) (not (response/full? x)))

(provide/contract
 [doctype string?]
 [build-no-cookie-response (not-response? . -> . response?)]
 [build-cookie-response (any/c (listof any/c) . -> . response?)]
 [build-cookie-forwarder (string? (listof any/c) . -> . response?)]
 [request-cookies* (request? . -> . (listof client-cookie?))])



;; ============================================================
;; utilities for setting cookies

(define (set-when-true fn val)
  (if val
      (λ (c) (fn c val))
      (λ (c) c)))

(define-syntax o
  (syntax-rules ()
    [(o f) f]
    [(o f f2 ...) (lambda (x) (o* x f f2 ...))]))

(define-syntax o*
  (syntax-rules ()
    [(o* x) x]
    [(o* x f g ...) (f (o* x g ...))]))


(define (build-cookie name val
                      #:comment [comment #f]
                      #:domain  [domain #f]
                      #:max-age [max-age #f]
                      #:path    [path #f]
                      #:secure? [secure? #f])
  ((o (set-when-true cookie:add-comment comment)
      (set-when-true cookie:add-domain domain)
      (set-when-true cookie:add-max-age max-age)
      (set-when-true cookie:add-path path)
      (set-when-true cookie:secure secure?))
   (set-cookie name val)))

;; cookie->env-binding : cookie -> (cons Symbol String)
;; gets the environment binding that will set the given cookie
(define (cookie->env-binding cookie)
  (make-header #"Set-Cookie" (string->bytes/utf-8 (print-cookie cookie))))

;; build-cookie-response : xexpr[xhtml] (listof cookie) -> response
(define (build-no-cookie-response xexpr)
  (make-response/full 200
                      "Okay"
                      (current-seconds)
                      TEXT/HTML-MIME-TYPE
		      '()
                      (list doctype (xexpr->string xexpr))))

;; build-cookie-response : xexpr[xhtml] (listof cookie) -> response
(define (build-cookie-response xexpr cookies)
  (make-response/full 200
                      "Okay"
                      (current-seconds)
                      TEXT/HTML-MIME-TYPE
                      (map cookie->env-binding cookies) ; rfc2109 also recommends some cache-control stuff here
                      (list doctype (xexpr->string xexpr))))

(define (build-cookie-forwarder url cookies)
  (make-response/full 303
                      "See Other"
                      (current-seconds)
                      TEXT/HTML-MIME-TYPE
                      (cons
                       (make-header #"Location" (string->bytes/utf-8 url))
                       (map cookie->env-binding cookies))
                      (list
                       doctype
                       (xexpr->string `(html (body "Resource redirected to " (a ((href ,url)) ,url)))))))

;; ============================================================
;; utilities for retrieving cookies

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

#|
   cookie          =       "Cookie:" cookie-version
                           1*((";" | ",") cookie-value)
   cookie-value    =       NAME "=" VALUE [";" path] [";" domain]
   cookie-version  =       "$Version" "=" value
   NAME            =       attr
   VALUE           =       value
   path            =       "$Path" "=" value
   domain          =       "$Domain" "=" value
|#
(define-lex-abbrevs
  (tspecial (:or (char-set "()<>@,;:\\\"/[]?={}") whitespace #\tab))
  (token-char (:- any-char tspecial iso-control)))

(define-tokens regular (TOKEN QUOTED-STRING))
(define-empty-tokens keywords (EQUALS SEMI COMMA PATH DOMAIN VERSION EOF))

(define cookie-lexer
  (lexer
   [(eof) (token-EOF)]
   [whitespace (cookie-lexer input-port)]
   ["=" (token-EQUALS)]
   [";" (token-SEMI)]
   ["," (token-COMMA)]
   ["$Path" (token-PATH)]
   ["$Domain" (token-DOMAIN)]
   ["$Version" (token-VERSION)]
   [(:: #\" (:* (:or (:~ #\") "\\\"")) #\")
    (token-QUOTED-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ token-char) (token-TOKEN lexeme)]))

(define assoc-list-parser
  (parser (start items)
          (tokens regular keywords)
          (grammar (items [(item separator items) (cons $1 $3)]
                          [(item) (list $1)])
                   (separator
                    [(COMMA) #t]
                    [(SEMI) #t])
                   (item [(lhs EQUALS rhs) (cons $1 $3)])
                   (lhs [(DOMAIN) 'domain]
                        [(PATH) 'path]
                        [(TOKEN) $1])
                   (rhs [(TOKEN) $1]
                        [(QUOTED-STRING) $1]))
          (end EOF)
          (error (lambda (a b c) (error 'assoc-list-parser "Malformed cookie: ~v ~v ~v" a b c)))))

(define (do-parse str)
  (with-handlers ([exn:fail? 
                   (λ (e) '())])
    (let ([ip (open-input-string str)])
      (dynamic-wind
       void
       (λ () (raw->cookies (assoc-list-parser (λ () (cookie-lexer ip)))))
       (λ () (close-input-port ip))))))

;; raw->cookies : flat-property-list -> (listof cookie)
(define (raw->cookies associations)
  
  ;; get-cookie-setter : symbol -> cookie string -> cookie
  ;; gets a setter for the given property
  (define (get-cookie-setter property-name)
    (case property-name
      [(domain) (λ (c x) (set-client-cookie-domain! c x) c)]
      [(path)   (λ (c x) (set-client-cookie-path! c x) c)]
      [else     (λ (c x) c)]))
  
  (unless (and (pair? associations) (string? (car (car associations))))
    (error 'raw->cookies "expected a non-empty association list headed by a cookie"))
  
  (let loop ([l (cdr associations)]
             [c (make-client-cookie (string->symbol (car (car associations)))
                                    (cdr (car associations))
                                    #f #f)])
    (cond
      [(null? l) (list c)]
      [(string? (car (car l)))
       (cons c (loop (cdr l) (make-client-cookie
                              (string->symbol (car (car l)))
                              (cdr (car l))
                              #f #f)))]
      [else
       (loop (cdr l)
             ((get-cookie-setter (car (car l))) c (cdr (car l))))])))

;; request-cookies* : request -> (listof cookie)
(define (request-cookies* req)
  (let* (; i don't see anything that says the client has to send at most one cookie: header,
         ; though firefox seems to always do so
         [cookie-strs (extract-bindings 'cookie (request-headers req))]
         [cookies (apply append (map do-parse cookie-strs))])
    cookies))

;; request-cookies : request -> env
(define (request-cookies req)
  (let ([cookies (request-cookies* req)])
    (map (λ (c) (cons (client-cookie-name c) (client-cookie-value c))) cookies)))
