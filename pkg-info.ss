#lang scheme

(require 
 web-server/managers/none
 web-server/servlet
 "configuration.ss"
 "data-structures.ss"
 "db.ss"
 "html.ss")

(define instance-expiration-handler #f)
(define manager
  (create-none-manager instance-expiration-handler))
(define interface-version 'v2)
(provide interface-version manager start)

(define (start req)
  (define (default-exception-handler e)
    (let ([exception-message 
           (let ([op (open-output-string)])
             (parameterize ([current-error-port op])
               ((error-display-handler) (exn-message e) e))
             (get-output-string op))])
      (begin
        (log-error 
         (request-client-ip req)
         (format "unhandled exception: ~a" exception-message))
        (mkdisplay
         '("Error")
         `((p 
            "Oops! The PLaneT server encountered an internal error and could not process your request. The error has been logged, "
            "but you may get in touch with us at planet@racket-lang.org if you would like to tell us more about it.")
           ,@(if (DISPLAY-ERRORS-OVER-WEB?)
                 `((p "The error message was: ")
                   (pre ,exception-message))
                 '()))
         req))))
  
  (define (find-em-packages)
    (let loop ([cats (get-package-listing 3)])
      (cond
       [(null? cats) '()]
       [else (append (category->packages (car cats))
		     (loop (cdr cats)))])))

  (define (category->packages cat) 
    (map (lambda (package)
	   (map (lambda (f) (f package))
		(list package-owner
		      package-name
		      get-latest-version)))
	 (category-packages cat)))

  (define (get-latest-version package) 
    (define versions (package-versions package))
    (if (null? versions)
	#f
	(list (pkgversion-maj (car versions))
	      (pkgversion-min (car versions)))))

  (with-handlers
      ([exn:user? (λ (e) (mkhtmlpage '("Error") `((div ((class "error")) ,(exn-message e)))))]
       [exn:fail? default-exception-handler])
      `(pre
	,(pretty-format (find-em-packages)))))
