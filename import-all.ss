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
     ("ams"          "Adam Shaw"               "adamshaw@cs.uchicago.edu")
     ("cce"          "Carl Eastlund"           "cce@ccs.neu.edu")
     ("cdutchyn"     "Christopher Dutchyn"     "cdutchyn@cs.ubc.ca")
     ("clements"     "John Clements"           "clements@brinckerhoff.org")
     ("cobbe"        "Richard Cobbe"           "cobbe@ccs.neu.edu")
     ("daedalus"     "Ryan Kaulakis"           "rmk216@elvis.arl.psu.edu")
     ("dfriedman"    "Dan Friedman"            "jacobm+kanren@cs.uchicago.edu")
     ("dherman"      "Dave Herman"             "dherman@ccs.neu.edu")
     ("dignatof"     "Daniel Ignatoff"         "Daniel_Ignatoff@brown.edu")
     ("divascheme"   "DivaScheme"              "dyoo@hkn.eecs.berkeley.edu")
     ("dvanhorn"     "David Van Horn"          "dvanhorn@cs.brandeis.edu")
     ("dyoo"         "Danny Yoo"               "dyoo@hkn.eecs.berkeley.edu")
     ("evanfarrer"   "Evan Farrer"             "evan.farrer@gmail.com")
     ("jacobm"       "Jacob Matthews"          "jacobm@cs.uchicago.edu")
     ("jaymccarthy"  "Jay McCarthy"            "jaymccarthy@cs.brown.edu")
     ("jim"          "Jim Bender"              "jim@benderweb.net")
     ("kazzmir"      "Jon Rafkind"             "workmin@ccs.neu.edu")
     ("lizorkin"     "Dmitry Lizorkin"         "lizorkin@ispras.ru")
     ("lshift"       "LShift Ltd."             "tonyg@lshift.net")
     ("mato"         "Robert Matovinovic"      "robert.matovinovic@web.de")
     ("mburns"       "Mike Burns"              "netgeek@speakeasy.net")
     ("neil"         "Neil Van Dyke"           "neil@neilvandyke.org")
     ("oesterholt"   "Hans Oesterholt-Dijkema" "hans@oesterholt.net")
     ("pjmatos"      "Paulo Jorge Matos"       "pocm@soton.ac.uk")
     ("planet"       "PLaneT"                  "jacobm+planet@cs.uchicago.edu")
     ("plt"          "PLT Scheme"              "samth@ccs.neu.edu")
     ("robby"        "Robby Findler"           "robby@cs.uchicago.edu")
     ("ryanc"        "Ryan Culpepper"          "ryanc@ccs.neu.edu")
     ("samth"        "Sam Tobin-Hochstadt"     "samth@ccs.neu.edu")
     ("schematics"   "Schematics"              "noelwelsh@yahoo.com")
     ("soegaard"     "Jens Axel Soegaard"      "jensaxel@soegaard.net")
     ("sweeney"      "Corey Sweeney"           "corey.sweeney@gmail.com")
     ("untyped"      "Untyped"                 "noelwelsh@yahoo.com")
     ("williams"     "Douglas Williams"        "M.DOUGLAS.WILLIAMS@saic.com")
     ("wmfarr"       "Will M. Farr"            "farr@MIT.EDU")
     ("zck"          "Chongkai Zhu"            "czhu@cs.utah.edu")))
  
  
  
  
  
  
  
  )