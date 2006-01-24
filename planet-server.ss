#| planet-server.ss -- PLaneT server

Serves up fresh PLaneT files, steaming hot

Follows the protocol listed in the PLaneT client file

|#
(module planet-server mzscheme
  
  (require 
   (lib "match.ss")
   (lib "port.ss")
   (lib "planet-shared.ss" "planet" "private")
   (only (lib "config.ss" "planet") CACHE-DIR)
   
   "server-config.ss"
   "logging.ss")
  
  (provide handle-one-request start)
  
  ;; ============================================================
  ;; BASIC SERVER MECHANISMS

  ;; handle-one-one-request : string,                             [language version for package]
  ;;                          pkg-spec,                           [package's specification]
  ;;                          (pkg nat nat path -> void),         [function to call to transmit a file]
  ;;                          (pkg-spec symbol string -> void),   [function to call to report a failure]
  ;;                          (-> void),                          [continuation to call if processing can continue]
  ;;                          (-> void)                           [continuation to call if processing must stop]
  ;; ->
  ;; void
  (define (handle-one-request language-version pkg-spec transmit-file transmit-failure proceed-k stop-k)
    (if (legal-language? language-version)
        (let* ([repository (build-path 
                            (PLANET-SERVER-REPOSITORY) 
                            (language-version->repository language-version))]
               [cache-pkg (parameterize ((CACHE-DIR repository))
                            (lookup-package pkg-spec))])
          (if cache-pkg
              (let* ([path (pkg-path cache-pkg)]
                     [maj (pkg-maj cache-pkg)]
                     [min (pkg-min cache-pkg)]
                     [file (build-path path (pkg-spec-name pkg-spec))])
                (if (file-exists? file)
                    (begin
                      (transmit-file cache-pkg maj min file)
                      (proceed-k))
                    (begin
                      (transmit-failure pkg-spec 'not-found "Internal error: inconsistent server state")
                      (stop-k))))
              (begin
                (transmit-failure pkg-spec 'not-found "No package matched the specified criteria")
                (proceed-k))))
        (begin
          (transmit-failure pkg-spec 'bad-language (format "Unknown package language: ~s" language-version))
          (stop-k))))
  
  ;; ============================================================
  ;; THE PLANET PROTOCOL
  
  (define VERSION-STRING "PLaneT/1.0")

  ; start : -> [diverges]
  ; listens for PLaneT requests and handles them
  (define (start)
    (let ((listener (tcp-listen (PLANET-SERVER-PORT) 4 #t)))
      (repeat-forever 
       (lambda () 
         (let-values ([(ip op) (tcp-accept listener)])
           (file-stream-buffer-mode op 'line)
           (thread 
            (lambda () 
              (with-handlers ([exn:fail? (void)])
                (handle ip op)))))))))
  
  ; handle : input-port output-port -> -> void
  ; handles protocol actions coming in from the given input port
  (define (handle ip op)
    
    ;; exceptions raised here will be handled by the calling loop
    (define-values (server-ip-address client-ip-address)
      (if (tcp-port? ip)
          (tcp-addresses ip)
          (values "<unknown>" "<unknown>")))
    
    (define (close-ports)
      (close-input-port ip)
      (close-output-port op))
    
    ; transmit-file : Nat FULL-PKG Nat Nat string[filename] -> void
    ; transmits the file named by the given string over op. The given number is the
    ; transaction's sequence number.
    (define ((transmit-file seqno) thepkg maj min file)
      (let ([bytes (file-size file)]
            [file-port (open-input-file file)])
        (write-line (list seqno 'get 'ok maj min bytes) op)
        (with-handlers ([exn:fail?
                         (lambda (e) (log-error client-ip-address 'transmission-failure (exn-message e)))])
          (copy-port file-port op)
          (log-download client-ip-address (pkg-path thepkg) (pkg-name thepkg) maj min))))
    
    (define (gettable-error? err)
      (case err
        [(not-found) #t]
        [(bad-language) #f]
        [else (error 'gettable-error ("unknown error code ~s" err))]))
          
    
    ; transmit-failure : Nat PKG-SPEC ERROR-CODE string -> void
    ; reports a failure to handle a get request
    (define ((transmit-failure seqno) thepkg error-code msg)
      (if (gettable-error? error-code)
          (write-line (list seqno 'get 'error error-code msg) op)
          (write-line (list seqno 'error error-code msg) op))
      (log-error client-ip-address error-code 
                 (parameterize ((print-struct #t))
                   (format "~a: ~s" msg thepkg))))
    
    (define (state:initialize)
      (let ((version (read-line ip)))
        (cond
          [(string=? version VERSION-STRING)
           (write-line 'ok op)
           (state:get-requests)]
          [else
           (write-line `(invalid ,(format "This server uses only the ~a protocol" VERSION-STRING)) op)
           (log-error client-ip-address 'wrong-protocol-version version)
           (close-ports)])))
    
    (define (nat-or-false? n) (or (not n) (nat? n)))
    

    (define (state:get-requests)
      (let ((request (read ip)))
        (match request
          [((? nat? seqno) 
            'get 
            (? string? language-version)
            (? string? name)
            (? nat-or-false? maj) 
            (? nat-or-false? min-lo)
            (? nat-or-false? min-hi) 
            (? string? path) ...)
           (handle-one-request language-version
                               (make-pkg-spec name maj min-lo min-hi path #f language-version)
                               (transmit-file seqno)
                               (transmit-failure seqno)
                               state:get-requests
                               void)]
          [((? nat? seqno) _ ...)
           (write-line (list seqno 'error 'malformed-request (format "Unknown request: ~s" request)) op)
           (log-error client-ip-address 'malformed-request request)
           (state:get-requests)]
          ['end 
            (close-ports) ; this is okay only because we're not multithreading the request-handling
            ]
          [_ 
           (write-line (list 'error 'malformed-input (format "Not a request: ~a" request)) op)
           (log-error client-ip-address 'malformed-input request)
           (close-ports)])))

    (state:initialize)))