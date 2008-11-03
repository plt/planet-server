#lang scheme/base

#|

This cuts out unrecognized html tags and attributes from xexprs to try to prevent XSS attacks.

|#

(require scheme/match)
(provide sanitize)

(define good-tags
  '(a abbr acronym address area b big blockquote br button caption center cite code col colgroup dd del dfn dir div dl dt em fieldset font form h1 h2 h3 h4 h5 h6 hr i img input ins kbd label legend li map menu ol optgroup option p pre q s samp select small span strike strong sub sup table tbody td textarea tfoot th thead tr tt u ul var))

(define good-attributes
  '(abbr accept accept-charset accesskey action align alt axis border cellpadding cellspacing char charoff charset checked cite class clear cols colspan color compact coords datetime dir disabled enctype for frame headers height href hreflang hspace id ismap label lang longdesc maxlength media method multiple name nohref noshade nowrap prompt readonly rel rev rows rowspan rules scope selected shape size span src start summary tabindex target title type usemap valign value vspace width))

(define tag-ht (make-hash))
(for-each (λ (x) (hash-set! tag-ht x #t)) good-tags)
(define (good-tag? x) (hash-ref tag-ht x #f))

(define attr-ht (make-hash))
(for-each (λ (x) (hash-set! attr-ht x #t)) good-attributes)
(define (good-attribute? x) (hash-ref attr-ht x #f))

(define (sanitize xexpr)
  (match xexpr
    [`(pre ,attrs ,stuff ...) 
     `(tt ,@(apply append (map split-newlines stuff)))]
    [`(,tag ,attrs ,stuff ...)
     (if (good-tag? tag)
         `(,tag ,(filter-attrs attrs) ,@(map sanitize stuff))
         `(div ,@(map sanitize stuff)))]
    [else xexpr]))

(define (filter-attrs attrs)
  (filter (λ (x) (good-attribute? (car x))) attrs))

(define (split-newlines stuff)
  (cond
    [(string? stuff) 
     (let ([strs (regexp-split #rx"\n" stuff)])
       (if (or (null? strs) (null? (cdr strs)))
           strs
           (let loop ([str (car strs)]
                      [strs (cdr strs)])
             (cond [(null? strs) (list str)]
                   [else (list* str '(br) (loop (car strs) (cdr strs)))]))))]
    [else (list stuff)]))
