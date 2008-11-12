(module planet-servlet mzscheme
  
  ;; this servlet implements the PLaneT response mechanism over HTTP
  ;; it does not support all the features planned for the full
  ;; PLaneT protocol (in particular it won't do negotiation) but
  ;; then again the regular planet server doesn't either so why
  ;; make firewalls angry?
  
  (require (only "planet-server.ss" handle-one-request)
           "db.ss"
           "data-structures.ss"
           (all-except (lib "planet-shared.ss" "planet" "private") legal-language?)
           
           (lib "servlet.ss" "web-server")
           web-server/managers/none
           (lib "string.ss"))
  
  (define instance-expiration-handler #f)
  (define manager
    (create-none-manager instance-expiration-handler))
  (define interface-version 'v2)
  (provide interface-version manager start)
  
  (define (nat-or-false? x) (or (not x) (nat? x)))
  (define (list-of-strings? v)
    (and (list? v) (andmap string? v)))
  
  ; start : request -> response
  (define (start initial-request)
    
    ;; error-code->status-code : error-code -> (list number string)
    ;; get the HTTP status code that goes with the given error
    (define (error-code->status-code err)
      (case err
        [(not-found)    (list 404 "Not Found")]
        [(bad-language) (list 400 "Bad Request")]
        [(malformed-input) (list 400 "Bad Request")]
        [else (error 'error-code->status-code (format "Unknown error code: ~s" err))]))
    
    
    ; transmit-failure/exit : PKG-SPEC ERROR-CODE string -> void
    ; reports a failure to handle a get request
    (define (transmit-failure/exit thepkg error-code msg)
      (log-error (request-client-ip initial-request) #;error-code ; don't record the error code (it's useless at the moment)
                 (parameterize ((print-struct #t))
                   (format "~a: ~s" msg thepkg)))
      (let ((status-code (error-code->status-code error-code)))
        (send/back
         (make-response/full
          (car status-code)
          (cadr status-code)
          (current-seconds)
          #"text/plain"
          '()
          (list msg)))))
    
    
    (with-handlers ([exn? (lambda (e)
                            (send/back
                             (make-response/full
                              500
                              "PLaneT server error"
                              (current-seconds)
                              #"text/plain"
                              '()
                              (list (exn-message e)))))])
      
      (startup)
      (let* ([bindings (request-bindings initial-request)]
             [get (lambda (n ok?)
                    (let ((v (with-handlers ([exn? (λ (e) (transmit-failure/exit #f
                                                                                 'malformed-input
                                                                                 (format "Binding for ~a had improper format" n)))])
                               (read-from-string (extract-binding/single n bindings)))))
                      (if (ok? v)
                          v
                          (transmit-failure/exit #f 'malformed-input (format "Binding for ~a had improper format" n)))))])
        (let ([language-version (get 'lang string?)]
              [name             (get 'name string?)]
              [maj              (get 'maj nat-or-false?)]
              [min-lo           (get 'min-lo nat-or-false?)]
              [min-hi           (get 'min-hi nat-or-false?)]
              [path             (get 'path list-of-strings?)])
          (let ([; transmit-file : (pkgversion? path? . -> . any)
                 ; transmits the given package to the client.
                 transmit-file/exit
                 (lambda (thepkgver file)
                   (let-values ([(maj min)
                                 (if (pkgversion? thepkgver)
                                     (values (pkgversion-maj thepkgver)
                                             (pkgversion-min thepkgver))
                                     (apply values thepkgver))])
                     (when (pkgversion? thepkgver)
                       (log-download (request-client-ip initial-request) thepkgver language-version))
                     (send/back
                      (make-response/full
                       200
                       "Okay"
                       (current-seconds)
                       #"text/plain"
                       (list
                        (make-header #"Content-Length" (string->bytes/utf-8 (number->string (file-size file))))
                        (make-header #"Package-Major-Version" (string->bytes/utf-8 (number->string (pkgversion-maj thepkgver))))
                        (make-header #"Package-Minor-Version" (string->bytes/utf-8 (number->string (pkgversion-min thepkgver)))))
                       
                       (let ([file-port (open-input-file file)])
                         (begin0
                           (list (read-bytes (file-size file) file-port))
                           (close-input-port file-port)))))))])
            (handle-one-request
             language-version
             (make-pkg-spec name maj min-lo min-hi path #f language-version)
             transmit-file/exit
             transmit-failure/exit
             void
             void)))))))
