(module planet-servlet mzscheme
  
  ;; this servlet implements the PLaneT response mechanism over HTTP
  ;; it does not support all the features planned for the full 
  ;; PLaneT protocol (in particular it won't do negotiation) but
  ;; then again the regular planet server doesn't either so why
  ;; make firewalls angry?
    
  (require (only "planet-server.ss" handle-one-request) 
           "logging.ss"
           
           (lib "planet-shared.ss" "planet" "private")
           
           (lib "servlet.ss" "web-server")
           (lib "response.ss" "web-server")
           (lib "string.ss")
           (lib "port.ss"))
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (nat-or-false? x) (or (not x) (nat? x)))
  (define (list-of-strings? v)
    (and (list? v) (andmap string? v)))
  
  ; start : request -> response
  (define (start initial-request)
    
    ; transmit-file : Nat FULL-PKG Nat Nat string[filename] -> void
    ; transmits the file named by the given string over op. The given number is the
    ; transaction's sequence number.  
    ; [NOTICE: we log a successful download before we've actually done it. That's because of an
    ;  annoying problem with the server where we can't actually just generate the output we want
    ;  to send to the client ...]
    (define (transmit-file/exit thepkg maj min file)
      (log-download (request-client-ip initial-request) (pkg-path thepkg) (pkg-name thepkg) maj min)
      (send/finish
       (make-response/full
        200
        "Okay"
        (current-seconds)
        #"text/plain"
        `((Content-Length . ,(number->string (file-size file)))
          (Package-Major-Version . ,(number->string maj))
          (Package-Minor-Version . ,(number->string min)))

        (let ([file-port (open-input-file file)]
              [bytes (open-output-bytes)])
          (copy-port file-port bytes)
          (list (get-output-bytes bytes))))))
    
    ;; error-code->status-code : error-code -> (list number string)
    ;; get the HTTP status code that goes with the given error
    (define (error-code->status-code err)
      (case err
        [(not-found)    (list 404 "Not Found")]
        [(bad-language) (list 400 "Bad Request")]
        [(malformed-input) (list 400 "Bad Request")]
        [else (error 'error-code->status-code (format "Unknown error code: ~s" err))]))
    
    
    ; transmit-failure : Nat PKG-SPEC ERROR-CODE string -> void
    ; reports a failure to handle a get request
    (define (transmit-failure/exit thepkg error-code msg)
      (log-error (request-client-ip initial-request) error-code
                 (parameterize ((print-struct #t))
                   (format "~a: ~s" msg thepkg)))
      (let ((status-code (error-code->status-code error-code)))
        (send/finish
         (make-response/full
          (car status-code)
          (cadr status-code)
          (current-seconds)
          #"text/plain"
          '()
          (list msg)))))  
    
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
          (handle-one-request 
           language-version
           (make-pkg-spec name maj min-lo min-hi path #f)
           transmit-file/exit
           transmit-failure/exit
           void
           void))))

  )