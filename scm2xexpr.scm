#|
   Copyright (c) 1997-2003, Dorai Sitaram.
   Portions copyright (c) 2004, Anton van Straaten.
   All rights reserved.

   Permission to distribute and use this work for any
   purpose is hereby granted provided this copyright
   notice is included in the copy.  This work is provided
   as is, with no warranty of any kind.
|#

(module scm2xexpr mzscheme

  (provide scheme-text->xexpr
           scheme-file->xexpr
           scheme->xexpr)

  (require "scheme-names.scm")

  ; constants
  (define *invisible-space* (list '*invisible-space*))
  (define *return* (integer->char 13))
  (define *scm-token-delims*
    (list #\( #\) #\[ #\] #\{ #\} #\' #\` #\" #\; #\, #\|))

  (define *scm-variables* '())

  (define scheme-file->xexpr
    (lambda (filename . stylesheet-name)
      (apply scheme->xexpr filename 'file stylesheet-name)))

  (define scheme-text->xexpr
    (lambda (source-text . stylesheet-name)
      (apply scheme->xexpr source-text 'source stylesheet-name)))

  ; scheme->xexpr : string * symbol [* string] -> xexpr
  (define scheme->xexpr
    (lambda (input-source input-type . stylesheet-name)
      (apply maybe-wrap-in-page
             `(div ([class "scheme"]) ; use div so stylesheet can apply margin
                   (pre               ; can't use class here, twiki won't notice pre tag
                    ,@((make-input-stream input-source input-type) 
                       (lambda (get-char toss-back-char)
                         ((make-formatter get-char toss-back-char)
                          (lambda (snoop get-next-xexpr)
                            (let loop ((output-elements '()))
                              (let ((c (snoop)))
                                (if (eof-object? c) 
                                    (reverse! output-elements)
                                    (loop (cons (get-next-xexpr) 
                                                output-elements)))))))))))
             stylesheet-name)))

  (define-syntax define-interface
    (syntax-rules (done:)
      ((_ name (op ...))
       (define name
         (lambda (op ...)
           (lambda (user)
             (user op ...)))))

      ((_ name (op ...) done: finalizer)
       (define name
         (lambda (op ... finalizer)
           (lambda (user)
             (begin0
               (user op ...)
               (finalizer))))))))

  ; input-stream interface idea inspired by Mayer Goldberg:
  ; http://www.cs.bgu.ac.il/~elhadad/advpro/2001/input-streams.scm
  ; Used here to wrap TeX2page input stream code, to reduce inter-
  ; dependencies, and separate input stream code from formatting code.
  (define-interface ^input-stream (get unget) done: close-input)
  (define-interface ^formatter (snoop-actual-char get-next-xexpr))

  ; creates an ^input-stream bound to the specified input source.  Wraps TeX2page input mechanism.
  (define make-input-stream
    (let* ([make-stream 
            (lambda (current-input close-input)
              (let ((input-line-no 1))
                (^input-stream
                 (lambda ()
                   (let ((b (bport-buffer current-input)))
                     (if (null? b)
                         (let ((p (bport-port current-input)))
                           (if (not p)
                               eof
                               (let ((c (read-char p)))
                                 (cond
                                   ((eof-object? c) c)
                                   ((char=? c #\newline)
                                    (set! input-line-no (+ input-line-no 1))
                                    c)
                                   (else c)))))
                         (let ((c (car b)))
                           (set-bport-buffer! current-input (cdr b))
                           c))))
                 
                 (lambda (c)
                   (set-bport-buffer!
                    current-input
                    (cons c (bport-buffer current-input))))
                 
                 close-input)))]

           [make-string-stream 
            (lambda (s)
              (make-stream (make-bport 'buffer (string->list s)) (lambda () #t)))])

      (lambda (source source-type)
        (case source-type
          ((file)   (if (file-exists? source)
                        (let ((inp (open-input-file source)))
                          (make-stream (make-bport 'port inp) (lambda () (close-input-port inp) #t)))
                        (make-string-stream (format ";*** Error: file not found: ~a\n" source))))

          ((source) (make-string-stream source))))))

  ; create a ^formatter interface based on the specified ^input-stream interface
  (define make-formatter
    (lambda (get-char toss-back-char)
      (letrec 
          ([scm-output-hash
            (lambda ()
              (get-actual-char)
              (let ((c (snoop-actual-char)))
                (cond
                  ((eof-object? c)
                   '(span ([class "selfeval"]) "#"))
                  ((char=? c #\|) (scm-output-extended-comment))
                  (else (toss-back-char #\#) (scm-output-token (scm-get-token))))))]
           
           [scm-output-next-chunk
            (lambda ()
              (let ((c (snoop-actual-char)))
                (cond
                  ((char=? c #\;) (scm-output-comment))
                  ((char=? c #\") (scm-output-string))
                  ((char=? c #\#) (scm-output-hash))
                  ((char=? c #\,)
                   (get-actual-char)
                   `(span ([class "keyword"])
                          ,(let ((c (snoop-actual-char)))
                             (cond 
                               ((char=? c #\@) (get-actual-char) ",@")
                               (else ",")))))
                  ((or (char=? c #\') (char=? c #\`))
                   (get-actual-char)
                   `(span ([class "keyword"]) 
                          ,(scm-emit-html-char c)))
                  ((or (char-whitespace? c) (memv c *scm-token-delims*))
                   (get-actual-char)
                   (scm-emit-html-char c))
                  (else (scm-output-token (scm-get-token))))))]
           
           [snoop-actual-char
            (lambda ()
              (let ((c (snoop-char)))
                (cond
                  ((eof-object? c) c)
                  ((invisible-space? c) (get-char) (snoop-actual-char))
                  ((char=? c *return*)
                   (get-char)
                   (let ((c (snoop-actual-char)))
                     (if (and (not (eof-object? c)) (char=? c #\newline))
                         c
                         (begin (toss-back-char #\newline) #\newline))))
                  (else c))))]
           
           [scm-output-comment
            (lambda ()
              `(span ([class "comment"])
                     ,(list->string 
                       (reverse!
                        (let loop ((comment-chars '()))
                          (let ((c (get-actual-char)))
                            (cond
                              ((eof-object? c) comment-chars)
                              ((char=? c #\newline) (cons c comment-chars))  ; final newline included in comment
                              ((and (char-whitespace? c)
                                    (let ((c2 (snoop-actual-char)))
                                      (or (eof-object? c2) (char=? c2 #\newline))))
                               (get-actual-char)
                               (cons #\newline comment-chars))               ; ditto here
                              (else (loop (cons c comment-chars))))))))))]
           
           [scm-output-extended-comment
            (lambda ()
              (get-actual-char)
              `(span ([class "comment"])
                     ,(string-append
                       "#|"
                       (list->string
                        (reverse!
                         (let loop ((comment-chars '()))
                           (let ((c (get-actual-char)))
                             (cond
                               ((eof-object? c) comment-chars)
                               ((char=? c #\|)
                                (let ((c2 (snoop-actual-char)))
                                  (cond
                                    ((eof-object? c2) comment-chars)
                                    ((char=? c2 #\#) (get-actual-char) comment-chars)
                                    (else (loop (cons c comment-chars))))))
                               (else (loop (cons c comment-chars))))))))
                       "|#")))]
           
           [scm-get-token
            (lambda ()
              (list->string
               (reverse!
                (let loop ((s '()) (esc? #f))
                  (let ((c (snoop-actual-char)))
                    (cond
                      ((eof-object? c) s)
                      (esc? (get-actual-char) (loop (cons c s) #f))
                      ((char=? c #\\) (get-actual-char) (loop (cons c s) #t))
                      ((or (char-whitespace? c) (memv c *scm-token-delims*)) s)
                      (else (get-actual-char) (loop (cons c s) #f))))))))]
           
           [snoop-char (lambda () (let ((c (get-char))) (toss-back-char c) c))]
           
           [get-actual-char
            (lambda ()
              (let ((c (get-char)))
                (cond
                  ((eof-object? c) c)
                  ((invisible-space? c) (get-actual-char))
                  ((char=? c *return*)
                   (let ((c (snoop-actual-char)))
                     (if (and (not (eof-object? c)) (char=? c #\newline))
                         (get-actual-char)
                         #\newline)))
                  (else c))))]
           
           [scm-output-string
            (lambda ()
              (get-actual-char) 
              `(span ([class "selfeval"])
                     ,(string-append
                       "\""
                       (list->string
                        (reverse!
                         (let loop ((s '()) (esc? #f))
                           (let ((c (get-actual-char)))
                             (case c
                               ((#\") (if esc? (loop (cons c s) #f)
                                          s))
                               ((#\\) (loop (cons c s) (not esc?)))
                               (else  (loop (cons c s) #f)))))))
                       "\"")))])
        
        (^formatter snoop-actual-char scm-output-next-chunk))))
  
  ; todo: add support for title
  (define maybe-wrap-in-page
    (lambda (xexpr . stylesheet-name)
      (if (null? stylesheet-name)
          xexpr
          `(html
            (head
             (link ([rel "stylesheet"]
                    [type "text/css"]
                    [href ,(car stylesheet-name)])))
            (body ,xexpr)))))
  
  (define scm-emit-html-char
    (lambda (c)
      (if (eof-object? c) '()
          (list->string (list c)))))
  
  (define scm-output-token
    (lambda (s)
      (let ((type (scm-get-type s)))
        (if (eq? type 'background) s
            `(span ([class ,(symbol->string type)]) ,s)))))
  
  (define member/string-ci=?
    (lambda (s ss) (ormap (lambda (x) (string-ci=? x s)) ss)))
  
  (define string-is-flanked-by-stars?
    (lambda (s)
      (let ((n (string-length s)))
        (and (>= n 3)
             (char=? (string-ref s 0) #\*)
             (char=? (string-ref s (- n 1)) #\*)))))
  
  (define string-starts-with-hash? (lambda (s) (char=? (string-ref s 0) #\#)))
  
  (define scm-get-type
    (lambda (s)
      (cond
        ((member/string-ci=? s *scm-keywords*) 'keyword)
        ((member/string-ci=? s *scm-builtins*) 'builtin)
        ((member/string-ci=? s *scm-variables*) 'variable)
        ((string-is-flanked-by-stars? s) 'global)
        (else
         (let ((colon (string-index s #\:)))
           (cond
             (colon (if (= colon 0) 'selfeval 'variable))
             ((string-is-all-dots? s) 'background)
             ((string-starts-with-hash? s) 'selfeval)
             ((string->number s) 'selfeval)
             (else 'variable)))))))
  
  (define-struct bport (port buffer))
  
  ; wrap plt constructor to match tex2page requirements
  (set! make-bport
        (let ((make-bport-orig make-bport))
          (lambda (field value)
            (apply make-bport-orig
                   (case field
                     ((buffer) (list #f value))
                     ((port)   (list value '())))))))
  
  (define invisible-space? (lambda (x) (eq? x *invisible-space*)))
  
  (define string-index
    (lambda (s c)
      (let ((n (string-length s)))
        (let loop ((i 0))
          (cond
            ((>= i n) #f)
            ((char=? (string-ref s i) c) i)
            (else (loop (+ i 1))))))))
  
  (define string-is-all-dots?
    (lambda (s)
      (let ((n (string-length s)))
        (let loop ((i 0))
          (cond
            ((>= i n) #t)
            ((char=? (string-ref s i) #\.) (loop (+ i 1)))
            (else #f))))))
  )