#| planet-server.ss -- PLaneT server

Serves up fresh PLaneT files, steaming hot

Follows the protocol listed in the PLaneT client file

|#
(module planet-server mzscheme
  
  (require 
   (lib "match.ss")
   (lib "port.ss")
   (lib "planet-shared.ss" "planet" "private")
   
   "server-config.ss")
  
  (provide start)
  
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
    (define-values (client-ip-address server-ip-address)
      (if (tcp-port? ip)
          (tcp-addresses ip)
          (values "<unknown>" "<unknown>")))
    
    (define (close-ports)
      (close-input-port ip)
      (close-output-port op))
    
    ; transmit-file : Nat FULL-PKG Nat Nat string[filename] -> void
    ; transmits the file named by the given string over op. The given number is the
    ; transaction's sequence number.
    (define (transmit-file seqno thepkg maj min file)
      (let ([bytes (file-size file)]
            [file-port (open-input-file file)])
        (write-line (list seqno 'get 'ok maj min bytes) op)
        (with-handlers ([exn:fail?
                         (lambda (e) (log-error client-ip-address 'transmission-failure (exn-message e)))])
          (copy-port file-port op)
          (log-download client-ip-address (pkg-path thepkg) (pkg-name thepkg) maj min))))
    
    ; transmit-failure : Nat FULL-PKG ERROR-CODE string -> void
    ; reports a failure to handle a get request
    (define (transmit-failure seqno thepkg error-code msg)
      (write-line (list seqno 'get 'error error-code msg) op)
      (log-error client-ip-address error-code msg))
    
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
           (if (legal-language? language-version)
               (match-let* ([thepkg (make-pkg-spec name maj min-lo min-hi path #f)]
                            [repository (build-path (PLANET-SERVER-REPOSITORY) 
                                                    (language-version->repository language-version))]
                            [cache-pkg (lookup-package thepkg repository)])
                 (if cache-pkg
                     (let* ([path (pkg-path cache-pkg)]
                            [maj (pkg-maj cache-pkg)]
                            [min (pkg-min cache-pkg)]
                            [file (build-path path (pkg-spec-name thepkg))])
                       (if (file-exists? file)
                           (transmit-file seqno cache-pkg maj min file)
                           (transmit-failure seqno thepkg 'not-found "Internal error: inconsistent server state")))
                     (transmit-failure seqno thepkg 'not-found "No package matched the specified criteria"))
                 (state:get-requests))
               (begin
                 (write-line (list seqno
                                   'error
                                   'bad-language 
                                   (format "Unknown package language: ~s" language-version))
                             op)
                 (log-error client-ip-address 'bad-language language-version)))]
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

    (printf "handling connection from ~a\n" client-ip-address)
    (state:initialize))
  
  ; log-download : string string string nat nat -> void
  ; logs the given download
  (define (log-download ip-addr owner pkg-name v-maj v-min)
    (record (list ip-addr owner pkg-name v-maj v-min) (PLANET-CONNECT-LOG)))
  
  ; log-error : string symbol tst ... -> void
  ; logs the given error. The extras should be information relevant to the given error type.
  (define (log-error ip-addr err-type . extras)
    (record (list* ip-addr err-type extras) (PLANET-ERROR-LOG)))
  
  (define (record msg file)
    (let ((op (open-output-file file 'append)))
      (fprintf op "~s\n" msg)
      (flush-output op)
      (close-output-port op)))
  
  )