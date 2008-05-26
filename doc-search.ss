#lang scheme/base

(require "html.ss")

(provide interface-version timeout start)
(define interface-version 'v1)
(define timeout +inf.0)

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
            "but you may get in touch with us at planet@plt-scheme.org if you would like to tell us more about it.")
           ,@(if (DISPLAY-ERRORS-OVER-WEB?)
                 `((p "The error message was: ")
                   (pre ,exception-message))
                 '()))
         req))))
  
  (with-handlers
      ([exn:user? (λ (e) (mkhtmlpage '("Error") `((div ((class "error")) ,(exn-message e)))))]
       [exn:fail? default-exception-handler])
    (mkdisplay '("Documentation Server")
               '((p "Sorry, the documentation server is not yet operational. For now, search your local PLT Scheme documentation instead."))
               req)))