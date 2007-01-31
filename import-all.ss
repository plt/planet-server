(module import-all mzscheme
  
  #| import-all.ss --- initial import of old planet files into the new server. |#
  (require "db.ss" "package-creation.ss" "configuration.ss")
  (require (lib "etc.ss") (lib "match.ss") (lib "list.ss"))

  (startup)
  (define default-repository-struct (car (get-all-repositories)))
  ;; ============================================================
  
  ;; path->users : path (string -> (list string[realname] string[email])) -> (listof  user)
  ;; read in a users record from an old-style planet repository
  (define (path->users path info-about)
    (listof-each 
     (directory-list path) (username-path)
     (where (directory-exists? (build-path path username-path)))
     (let ([username (path->string username-path)])
       (match-let ([`(,realname ,email) (info-about username)])
         (make-user 
          username
          realname
          email
          (listof-each 
           (directory-list (build-path path username-path)) (pkg-path)
           (unless (and (directory-exists? (build-path path username-path pkg-path))
                        (regexp-match #rx"^[a-zA-Z0-9-]*\\.plt$" (path->string pkg-path)))
             (error 'path->users "found a weird path: ~e" (build-path path username-path pkg-path)))
           (let* ([base (build-path path username-path pkg-path)]
                  [all-updates 
                   (apply append
                    (listof-each (directory-list base) (maj)
                     (where (number? (string->number (path->string maj))))
                     (listof-each (directory-list (build-path base maj)) (min)
                      (where (number? (string->number (path->string min))))
                      (make-update (string->number (path->string maj))
                                   (string->number (path->string min))
                                   (build-path base maj min pkg-path)))))]
                  [sorted-updates
                   (sort 
                    all-updates
                    (λ (a b) 
                      (or (< (update-maj a) (update-maj b))
                          (and (= (update-maj a) (update-maj b))
                               (< (update-min a) (update-min b))))))])
             (when (null? sorted-updates)
               (error 'path->users "something went wrong, because this path has no pkgversions: ~e" base))
             (make-package
              (path->string pkg-path)
              (update-file (car sorted-updates))
              (cdr sorted-updates)))))))))
                  
  ;; ============================================================
  
  ;; import proceeds as follows:
  ;;   for each user:
  ;;      - create user
  ;;      - for each package owned by user:
  ;;         * create the package
  ;;         * for each package update:
  ;;             # add the update, marking major or minor as appropriate
  
  (define (import-into-database users)
    (do-for-each users (u)
     (let ([user (create-new-user (user-name u) (user-realname u) (user-email u) (random-password))])
       (do-for-each (user-packages u) (p)
         (create-package user (package-name p) (get-package-bytes p) (list (DEFAULT-REPOSITORY)))
         (let ([package (get-package (user-name u) (package-name p))])
           (unless package
             (error 'import
                    "something went wrong: added package but then it didn't exist when I looked it up: ~e ~e"
                    (user-name u)
                    (package-name p)))
           (do-for-each (package-updates p) (update)
             (update-package user 
                             package
                             (backwards-compatible? update)
                             (get-update-bytes update)
                             (list default-repository-struct))))))))
  
  
  ;; ----------------------------------------
  (define-struct user (name      ; string 
                       realname  ; string
                       email     ; string
                       packages  ; (listof package)
                       ))
  (define-struct package (name    ; string
                          file    ; path
                          updates ; listof update
                          ))
  (define-struct update (maj      ; nat 
                         min      ; nat
                         file     ; path
                         ))
  
  ;; ----------------------------------------

  ;; get-package-bytes : package -> bytes
  ;; gets the bytes associated with this package (done on demand to avoid loading the whole repository in memory at once)
  (define (get-package-bytes p)
    (file->bytes (package-file p)))
    
  ;; ----------------------------------------  
  
  ;; backwards-compatible? : update -> boolean
  ;; determines if the given update is marked as backwards-compatible
  (define (backwards-compatible? u)
    (not (= (update-min u) 0)))

  ;; get-update-bytes : update -> bytes
  ;; gets the bytes corresponding to this update.
  (define (get-update-bytes u)
    (file->bytes (update-file u)))
  
  ;; ----------------------------------------
  
  ;; file->bytes : path -> bytes
  ;; reads the entire given file into a byte string
  (define (file->bytes file)
    (let ([len (file-size file)])
      (with-input-from-file file
        (λ () (read-bytes len)))))
  
  ;; random-password : -> string
  ;; generates a random password to seed the database
  (define (random-password)
    (list->string (build-list 8 (λ (_) (integer->char (random-int-in 32 127))))))
    
  ;; random-int-in : int int -> int
  ;; delivers a random int in the range [lo,hi)
  (define (random-int-in lo hi)
    (+ (random (- hi lo)) lo))
  
  ;; ============================================================
  ;; scaffolding
  
  (define-syntax do-for-each
    (syntax-rules ()
      [(do-for-each list-expr (name) body1 body2 ...)
       (for-each 
        (λ (name)
          body1 body2 ...)
        list-expr)]))
  
  (define-syntax listof-each
    (syntax-rules (where)
      [(listof-each list-expr (name) (where filter-expr) body1 body2 ...)
       (map
        (λ (name) body1 body2 ...)
        (filter
         (λ (name) filter-expr)
         list-expr))]
      [(listof-each list-expr (name) body1 body2 ...)
       (listof-each list-expr (name) (where #t) body1 body2 ...)]))
  
  ;; ============================================================
  ;; missing data
  
  (define-syntax info-fn
    (syntax-rules ()
      [(info-fn (e1 e2 ...) ...)
       (let ([ht
              (hash-table 'equal
                (e1 (list e2 ...)) ...)])
         (λ (arg)
           (hash-table-get ht arg (λ () (list "unknown" "jacobm@cs.uchicago.edu")))))]))
  
  (define info
    (info-fn
     ("ams"          "known person" "jacobm+known@cs.uchicago.edu")
     ("cce"          "known person" "jacobm+known@cs.uchicago.edu")
     ("cdutchyn"     "known person" "jacobm+known@cs.uchicago.edu")
     ("clements"     "known person" "jacobm+known@cs.uchicago.edu")
     ("cobbe"        "known person" "jacobm+known@cs.uchicago.edu")
     ("daedalus"     "known person" "jacobm+known@cs.uchicago.edu")
     ("dfriedman"    "known person" "jacobm+known@cs.uchicago.edu")
     ("dherman"      "known person" "jacobm+known@cs.uchicago.edu")
     ("dignatof"     "known person" "jacobm+known@cs.uchicago.edu")
     ("divascheme"   "known person" "jacobm+known@cs.uchicago.edu")
     ("dvanhorn"     "known person" "jacobm+known@cs.uchicago.edu")
     ("dyoo"         "known person" "jacobm+known@cs.uchicago.edu")
     ("evanfarrer"   "known person" "jacobm+known@cs.uchicago.edu")
     ("jacobm"       "known person" "jacobm+known@cs.uchicago.edu")
     ("jaymccarthy"  "known person" "jacobm+known@cs.uchicago.edu")
     ("jim"          "known person" "jacobm+known@cs.uchicago.edu")
     ("kazzmir"      "known person" "jacobm+known@cs.uchicago.edu")
     ("lizorkin"     "known person" "jacobm+known@cs.uchicago.edu")
     ("lshift"       "known person" "jacobm+known@cs.uchicago.edu")
     ("mato"         "known person" "jacobm+known@cs.uchicago.edu")
     ("mburns"       "known person" "jacobm+known@cs.uchicago.edu")
     ("neil"         "known person" "jacobm+known@cs.uchicago.edu")
     ("oesterholt"   "known person" "jacobm+known@cs.uchicago.edu")
     ("pjmatos"      "known person" "jacobm+known@cs.uchicago.edu")
     ("planet"       "known person" "jacobm+known@cs.uchicago.edu")
     ("plt"          "known person" "jacobm+known@cs.uchicago.edu")
     ("robby"        "known person" "jacobm+known@cs.uchicago.edu")
     ("ryanc"        "known person" "jacobm+known@cs.uchicago.edu")
     ("samth"        "known person" "jacobm+known@cs.uchicago.edu")
     ("schematics"   "known person" "jacobm+known@cs.uchicago.edu")
     ("soegaard"     "known person" "jacobm+known@cs.uchicago.edu")
     ("sweeney"      "known person" "jacobm+known@cs.uchicago.edu")
     ("untyped"      "known person" "jacobm+known@cs.uchicago.edu")
     ("williams"     "known person" "jacobm+known@cs.uchicago.edu")
     ("wmfarr"       "known person" "jacobm+known@cs.uchicago.edu")
     ("zck"          "known person" "jacobm+known@cs.uchicago.edu")))
  
  
  
  
  
  
  
  )