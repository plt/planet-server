#| a server package is the server's representation of an actual package
(as opposed to a request for a package). |#

(module server-package mzscheme

  (require
   (lib "planet-getinfo.ss")
   (lib "server-config.ss"))

  (define-struct pkg-spec (name     ; string        : name of package requested
                           maj      ; nat | #f      : major version requested. #f for latest.
                           minor-lo ; nat | #f      : lowest acceptable minor version. #f for 0.
                           minor-hi ; nat | #f      : highest acceptable minor version. #f for inf.
                           path     ; listof string : "logical" path to the resource, including owner
                           stx      ; syntax | #f   : syntax of the require form.
                           )
    (make-inspector))
  
  (define-struct package (name             ;; string          : name of package
                          path             ;; (listof string) : path to package, including owner
                          major-version    ;; nat 
                          minor-version    ;; nat
                          file-path        ;; path            : path to file containing the package
                          language-version ;; string          : version of mzscheme
                          getter           ;; symbol -> TST   : a get-info function
                          )
    (make-inspector))

;;;NOTE: it seems to me now that there should be a client-side
;;;architecture of package-notifier widgets that take a request and
;;;either satisfy it or forward it, getting a second crack at the
;;;answer once it comes back.

  ;; request->server-package : language-version pkg-spec -> package
  ;; finds the appropriate package to serve a request
  (define (request->server-package version pkg-spec)
    (lookup-package pkg-spec (PLANET-SERVER-REPOSITORY)))

  ;; info : package symbol X -> X
  ;; gets the metainfo associated with the given symbol
  ;; for this package. Returns the default if the tag doesn't exist.
  (define (info pkg sym default)
    ((package-getter pkg) sym default))
  
  ; language-version->repository : string -> string | #f
  ; finds the appropriate language version for the given repository
  (define (language-version->repository ver)
    (cond
      [(regexp-match #rx"20.+" ver) "207.1"]
      [(regexp-match #rx"3.+|29.|" ver) "300"]
      [else #f]))

  ; legal-language? : string -> bool
  ; determines if the given string is a legal language descriptor
  (define (legal-language? l)
    (and (language-version->repository l) #t))
  
  ; lookup-package : FULL-PKG-SPEC path -> PKG | #f
  ; returns the directory pointing to the appropriate package in the cache, or #f if the given package
  ; isn't in the cache
  (define (lookup-package pkg cache-dir)
    (let ((pkg-dir (build-path (apply build-path cache-dir (pkg-spec-path pkg)) (pkg-spec-name pkg))))
      (if (directory-exists? pkg-dir)
          (get-best-match pkg pkg-dir)
          #f)))
  
  ; get-best-match :FULL-PKG-SPEC path -> PKG | #f
  ; gets the best version in the given subdirectory in the specified low and high version range
  ; or #f if there is no appropriate version
  (define (get-best-match pkg-spec path)
    






    
    (let ((major-version (if (pkg-spec-maj pkg-spec)
                             (let ((specified-number (number->string (pkg-spec-maj pkg-spec))))
                               (if (directory-exists? (build-path path specified-number))
                                   specified-number
                                   #f))
                             (get-highest-numbered-subdir path #f #f))))
      (if major-version
          (let ((minor-version (get-highest-numbered-subdir 
                                (build-path path major-version)
                                (pkg-spec-minor-lo pkg-spec)
                                (pkg-spec-minor-hi pkg-spec))))
            (if minor-version
                (make-package
                 (pkg-spec-name pkg-spec)
                 (pkg-spec-path pkg-spec)
                 (string->number major-version) 
                 (string->number minor-version)
                 (build-path path major-version minor-version (pkg-)
                 
                
                #f))
          #f)))
  
  ; get-highest-numbered-subdir : path (Nat | #f) (Nat | #f) -> path | #f
  ; given a path, returns the subdirectory of that path with the highest numeric name or #f if
  ; none exists. Does not return the full path.
  (define (get-highest-numbered-subdir path lo hi)
    ; valid-dir? : path -> bool
    (define (valid-dir? d) 
      (and 
       (directory-exists? (build-path path d)) 
       (let ((n (string->number (path->string d))))
         (and n
              (or (not lo) (>= n lo))
              (or (not hi) (<= n hi))))))
    
    (unless (directory-exists? path) 
      (raise (make-exn:fail:filesystem 
              "Internal PLaneT error: inconsistent cache, directory does not exist" 
              (current-continuation-marks))))
    (max-string (map path->string (filter valid-dir? (directory-list path)))))
  
  
  )