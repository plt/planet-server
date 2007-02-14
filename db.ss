(module db mzscheme
  
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 1 2)))
  (require (lib "class.ss"))
  (require (lib "string.ss"))
  (require (lib "contract.ss"))
  (require (lib "xml.ss" "xml"))
  (require (lib "list.ss"))
  (require (lib "etc.ss"))
  (require (lib "match.ss"))
  (require (lib "pretty.ss"))
  
  (require "data-structures.ss" "configuration.ss" (prefix mantis: "mantis.ss"))

  (provide/contract
   
   [username-taken? (-> string? boolean?)]
   [email-taken? (-> string? boolean?)]
   [create-new-user (-> string? string? string? string? user?)]
   [get-user-record (-> string? string? (union user? false/c))]
   [get-logged-in-user-from-passcode (-> string? string? (union user? false/c))]
   [log-user-in (-> user? string?)]
   [log-user-out (-> user? void?)]
   [get-user-record/no-password (-> string? (union user? false/c))]
   [valid-password? (user? string? . -> . boolean?)]
   [update-user-password (user? string? . -> . void?)]
   [user->packages (-> user? natural-number/c (listof package?))]
   [get-category-names (-> (listof category?))]
   [add-package-to-db!
    (user? string? (or/c (listof xexpr?) false/c) (or/c string? false/c) . -> . package?)]
   [get-package-listing (natural-number/c . -> . (listof category?))]
   [get-matching-packages
    (string? string? string? (union natural-number/c false/c)
     natural-number/c (union natural-number/c false/c)
     . -> .
     (listof pkgversion?))]
   [get-package
    (opt-> (string? string?) (boolean?)
           (union package? false/c))]
   [get-package-by-id
    (-> natural-number/c natural-number/c
        (union package? false/c))]
   [get-package-version-by-id
    (-> natural-number/c natural-number/c (union pkgversion? false/c))]
   [reassociate-package-with-categories
    (package? (listof (or/c category? natural-number/c)) . -> . any)]
   [associate-package-with-category
    (package?
     (or/c category? natural-number/c)
     . -> .
     void?)]
   [get-package-categories (package? . -> . (listof category?))]
   [pkgversion->primary-files (pkgversion? . -> . (listof primary-file?))]
   [downloads-this-week (pkgversion? . -> . natural-number/c)]
   [get-all-repositories (-> (listof repository?))]
   [legal-repository? (-> number? boolean?)]
   [startup (-> void?)]
   [teardown (-> void?)]
   [add-pkgversion-to-db!
    (-> user? package? natural-number/c natural-number/c path? path? (-> symbol? (-> any) any)
        natural-number/c)]
   [update-package-fields!
    (-> package? 
        pkgversion?
        (union (listof xexpr?) false/c) ;; package blurb
        (union string? false/c)         ;; package homepage
        (union (listof xexpr?) false/c) ;; package notes
        (union string? false/c) 
        (union string? false/c)
        void?)]
   [associate-pkgversion-with-repository! (natural-number/c (union repository? natural-number/c) . -> . void?)]
   [get-next-version-number
    (-> package? boolean? (cons-immutable/c natural-number/c natural-number/c))]
   
   [log-download
    (string?  ; ip address
     pkgversion? ; the downloaded package
     string?  ; client core version
     . -> . void?)]
   [log-error
    (string?          ; ip address
     string?          ; freeform error message
     . -> . void?)]
   
   [for-each-package-version
    ((package? pkgversion? . -> . any) . -> . any)]
   
   [core-version-string->code (string? . -> . (union number? false/c))]
   [code->core-version-string (number? . -> . (union string? false/c))]
   )
  
  (provide sql-null?)
  
  ;; ============================================================
  ;; database setup
  ;; The strategy here is to create a connection, and refresh it
  ;; as necessary when a link dies for some reason. We use a proxy
  ;; object of class restarting-connection% for this purpose.
  
  (define retrying-connection%
    (class* object% ()
      (init-field get-conn)
      
      (define/public (perform-action query action)
        ;; the commented lines are timing data --- i really should figure out some
        ;; good way to log this all the time
        (let* (;[_ (printf "~s\n" query)]
               ;[start-time (current-milliseconds)]
               [conn (get-conn)])
          (begin0
            (action conn)
            (send conn disconnect)
            ;(printf "~a milliseconds\n\n" (- (current-milliseconds) start-time))
            )))
        
      (define/public (query-value q . args)
        (perform-action q (λ (c) (send/apply c query-value q args))))
      (define/public (query-tuple q . args)
        (perform-action q (λ (c) (send/apply c query-tuple q args))))
      (define/public (exec q . args)
        (perform-action q (λ (c) (send/apply c exec q args))))
      (define/public (map q . args)
        (perform-action q (λ (c) (send/apply c map q args))))
      (define/public (for-each q . args)
        (perform-action q (λ (c) (send/apply c for-each q args))))
      
      ;; not needed if there's no connection pool
      (define/public (disconnect)
        (void))
      
      (super-new)))
  
  (define *db* #f)
  (define (startup)
    (unless *db* 
      (set! *db* 
            (new retrying-connection%
                 [get-conn 
                  (λ () 
                    (let ([db (connect "localhost" 5432 "planet" "jacobm" "matrix")])
                      (send db use-type-conversions #t)
                      db))]))))
  (define (teardown)
    (when *db*
      (send *db* disconnect)
      (set! *db* #f)))
    
  ;; ============================================================
  ;; persistence functions
  
  
  ;; ------------------------------------------------------------
  ;; user creation/management
  
  ; username-taken? (-> string? boolean?)
  (define (username-taken? name)
    (let ([count (send *db* query-value (format "SELECT count(*) FROM contributors WHERE username = '~a'" name))])
      (not (= count 0))))
  
  ; email-taken? (-> string? boolean?)
  (define (email-taken? email)
    (let ([count (send *db* query-value (format "SELECT count(*) FROM contributors WHERE email = '~a'" email))])
      (not (= count 0))))
     
  ; create-new-user (-> string? string? string? string? void?)
  (define (create-new-user username realname email password)
    (let ([id (send *db* query-value "SELECT nextval('contributors_pk')")])
      (begin
        (send *db* exec
              (string-append
               "INSERT INTO contributors (id, username, realname, email, password) VALUES ("
               (number->string id)", "
               (escape-sql-string username)", "
               (escape-sql-string realname)", "
               (escape-sql-string email)", "
               "md5("(escape-sql-string password)"))"))
        (let ([user (make-user id username realname email)])
          (mantis:create-account user password)
          user))))
  
  (define (valid-password? user pass)
    (let ([query (string-append
                  "SELECT (password = md5("(escape-sql-string pass)")) AS valid "
                  "FROM contributors WHERE id = "(number->string (user-id user)))])
      (send *db* query-value query)))
  
  
  ;; update-user-password : user string -> void
  (define (update-user-password user newpass)
    (let ([sql (string-append
                "UPDATE contributors SET password = md5("(escape-sql-string newpass)") "
                "WHERE id = "(number->string (user-id user)))])
      (send *db* exec sql)
      (void)))
  
  
  ;; gets the user record corresponding to the given username and password,
  ;; or #f if the username/password combination is invalid
  (define (get-user-record username password)
    (let ([query (string-append
                  " SELECT id, username, realname, email FROM contributors "
                  " WHERE username = "(escape-sql-string username)
                  " AND password = md5("(escape-sql-string password)")")])
      (let ([result (send *db* map query make-user)])
        (if (null? result)
            #f
            (car result)))))
  
  ;; get-logged-in-user-from-passcode : string? string? -> (union user? false/c)
  ;; gets the user record for a user after validating the user's passcode, a one-time
  ;; code handed out to the user after login
  (define (get-logged-in-user-from-passcode username passcode)
    (let ([query (string-append
                  " SELECT id, username, realname, email FROM contributors "
                  " WHERE username = "(escape-sql-string username)
                  " AND passcode = "(escape-sql-string passcode))])
      (let ([result (send *db* map query make-user)])
        (if (null? result)
            #f
            (car result)))))
  
  ;; log-user-in : user? -> string?
  ;; creates a passcode for the given user and returns it
  (define (log-user-in u)
    (let* ([passcode (number->string (random 999999999))]
           [query (string-append
                   "UPDATE contributors SET passcode = "(escape-sql-string passcode)
                   " WHERE id = "(number->string (user-id u)))])
      (send *db* exec query)
      passcode))
  
  ;; log-user-out : user? -> void
  (define (log-user-out u)
    (let ([query (string-append
                  "UPDATE contributors SET passcode = NULL WHERE id = "(number->string (user-id u)))])
      (send *db* exec query)
      (void)))
  
  ;; like get-user-record, but does not require a password. For use with
  ;; passwordless authentication (e.g., resetting password with email confirmation)
  (define (get-user-record/no-password username)
    (let ([query (string-append
                  " SELECT id, username, realname, email FROM contributors "
                  " WHERE username = "(escape-sql-string username))])
      (let ([result (send *db* map query make-user)])
        (if (null? result)
            #f
            (car result)))))
  
  ;; ------------------------------------------------------------
  ;; package insertion/management
  (define (blurb-string->blurb b)
    (if b
        (let ([ip (open-input-string b)])
          (begin0
            (read ip)
            (close-input-port ip)))
        #f))
  
  (define (blurb->blurb-string b)
    (format "~s" b))
  
  (define (user->packages u rep)
    (let* ([query (string-append "SELECT * FROM packages_without_categories "
                                 " WHERE contributor_id = "(number->string (user-id u))
                                 " AND repository_id = "(number->string rep)
                                 " ORDER BY name")]
           [results (send *db* map query
                          (lambda row
                            (let ([f (λ (n) (fld (packages_without_categories) row n))])
                              (make-package
                               (f 'package_id)
                               (f 'username)
                               (f 'name)
                               (blurb-string->blurb (f 'pkg_blurb))
                               (f 'homepage)
                               (list (row->pkgversion (packages_without_categories) row (list rep)))
                               (f 'bugtrack_id)))))])
      results))
    
  ;; get-category-names : -> (listof category?)
  ;; produces the current list of all available categories and their corresponding ids,
  ;; sorted in presentation order
  (define (get-category-names)
    (let ([query "SELECT name, shortname, id FROM categories ORDER BY sort_order"])
      (send *db* map query (lambda (name shortname id) (make-category id name (string->symbol shortname) #f)))))
  
  ;; add-package-to-db! : user string (or (listof xexpr) #f) -> package?
  ;; adds a record for the given package information, and returns a stub
  (define (add-package-to-db! user package-name blurb homepage)
    (let* ([id (send *db* query-value "SELECT nextval('packages_pk') AS pk")]
           [query (string-append 
                   "INSERT INTO packages (id, owner_id, name, blurb, bugtrack_id) VALUES "
                   "("(number->string id)", "
                      (number->string (user-id user))", "
                      (escape-sql-string package-name)", "
                      (if blurb 
                          (escape-sql-string (format "~s" blurb))
                          "NULL")", "
                      "0" ;; dummy value that will be updated very shortly
                      ")")])
      (begin
        ;; FIXME: these two ought to be executed in the same transaction
        (send *db* exec query)
        (let* ([pkg (make-package id (user-username user) package-name blurb homepage '() #f)]
               [mantis-id (mantis:associate-user-with-package user pkg)])
          (send *db* exec 
                (string-append "UPDATE packages SET bugtrack_id = "(number->string mantis-id)
                               " WHERE id = "(number->string id)))
          (set-package-bugtrack-id! pkg mantis-id)
          pkg))))
                  
  
  ;; ------------------------------------------------------------
  ;; package lookup
  
  ;; matching-packages : string string string (union nat #f) [nat (union nat #f) | if maj \in nat] -> list pkg-version
  ;; gets all the matching package versions, sorted from best to worst
  ;; [note that ordering by hidden means that all the entries where hidden = true are after all the ones where
  ;;  it is false; this means that hidden packages are only selected if no non-hidden packages fit the request]
  (define (get-matching-packages requester-core-version pkgowner pkgname maj minlo minhi)
    (let* ([rc-version (number->string (core-version-string->code requester-core-version))]
           [query (string-append
                   "SELECT * FROM all_packages "
                   " WHERE (required_core_version <= "rc-version" OR required_core_version IS NULL) "
                   " AND name = "(escape-sql-string pkgname)
                   " AND username = "(escape-sql-string pkgowner)
                   (if maj 
                       (string-append " AND maj = "(number->string maj)" AND min >= "(number->string minlo)
                                      (if minhi (string-append " AND min <= "(number->string minhi)) ""))
                       "")
                   " AND repository_id = "(number->string (DEFAULT-REPOSITORY))
                   " ORDER BY hidden, maj DESC, min DESC;")])
      (send *db* map query 
            (lambda row 
              (let ([rep (fld (all_packages) row 'repository_id)])
                (row->pkgversion (all_packages) row (list rep)))))))
  
  ;; ----------------------------------------
  ;; table defs
  
  ;; ap-field : (listof tst)[row from all_packages view] symbol -> tst
  ;; gets the field named sym from row
  
  ;; get-column-names : symbol -> listof symbol
  ;; gets the names of all columns in the named table
  (define (get-column-names table-name)
    (let ([query (string-append "SELECT c.attname FROM pg_attribute c, pg_class t "
                                "WHERE c.attrelid = t.oid AND t.relname = "(escape-sql-string (symbol->string table-name))
                                "AND c.attisdropped = false and c.attnum >= 1 "
                                "ORDER BY c.attnum;")])
      (send *db* map query string->symbol)))
  
  (define (lazy tbl)
    (let ([columns #f])
      (lambda ()
        (unless columns
          (set! columns (get-column-names tbl)))
        columns)))
  
  (define-values (all_packages 
                  all_packages_without_repositories
                  most_recent_packages
                  packages_with_categories
                  packages_without_categories)
    (apply values (map lazy '(all_packages 
                              all_packages_without_repositories
                              most_recent_packages
                              packages_with_categories
                              packages_without_categories))))
  
  (define (fld table-cols row column)
    (let ([ans (list-ref row (idx-of column table-cols))])
      (if (sql-null? ans)
          #f
          ans)))
  
  (define (idx-of s l)
    (let loop ([n 0] [l l])
      (cond
        [(null? l) (error 'idx-of "couldn't find item in list: ~e" s)]
        [(eq? s (car l)) n]
        [else (loop (add1 n) (cdr l))])))
  
  ;; row->pkgversion : (listof TST)[row] (listof symbol)[column names for the given row] -> pkgversion?
  ;; converts row into a pkgversion structure. RUNS AN AUXILIARY DB QUERY to determine
  ;; repositories.
  (define (row->pkgversion columns row repositories)
    (let* ([id (fld (all_packages) row 'package_version_id)]
           #;[repositories (send *db* map 
                               (string-append "SELECT repository_id FROM version_repositories WHERE package_version_id = "(number->string id))
                               (lambda (x) x))]
           [f (lambda (col) (fld columns row col))])
      (make-pkgversion
       (f 'package_version_id)
       (f 'package_id)
       (f 'maj)
       (f 'min)
       (string->path (f 'plt_path))
       (string->path (f 'src_path))
       (f 'default_file)
       (f 'doctxt)
       (blurb-string->blurb (f 'release_blurb))
       (f 'version_date)
       (f 'version_name)
       repositories
       (let ([s (f 'required_core_version)])
         (if s
             (code->core-version-string s)
             #f))
       (f 'downloads))))
  
  ;; get-package : string string [boolean] -> (union package #f)
  ;; gets the given package by owner and package name
  ;; [could be reduced to get-package-by-id, but i'm a little concerned about the number
  ;;  of queries I'm generating at the moment]
  (define get-package
    (opt-lambda (owner name [include-hidden? #f])
      (let* ([query
              (string-append
               "SELECT * FROM all_packages "
               " WHERE username = "(escape-sql-string owner)
               " AND name = "(escape-sql-string name)
               (if include-hidden?
                   ""
                   " AND hidden = false")
               " ORDER BY maj DESC, min DESC")]
             [pkgversions (send *db* map query list)])
        (version-rows->package (all_packages) pkgversions))))
  
  ;; get-package-by-id : nat nat -> (union package #f)
  ;; gets the given package id, but only if its owner is the given owner
  (define (get-package-by-id pkg-id user-id)
    (let* ([query 
            (string-append
             "SELECT * FROM all_packages WHERE contributor_id = "(number->string user-id)
             " AND package_id = "(number->string pkg-id)
             " ORDER BY maj DESC, min DESC")]
           [pkgversions (send *db* map query list)])
      (version-rows->package (all_packages) pkgversions)))
         
  ;; this function relies on the idea the rows passed in will be from a derivative of the all_packages
  ;; query and at least have its rows as a prefix
  (define (version-rows->package columns pkgversions)
    (cond
      [(null? pkgversions) #f]
      [else
       (make-package
        (fld columns (car pkgversions) 'package_id)
        (fld columns (car pkgversions) 'username)
        (fld columns (car pkgversions) 'name)
        (blurb-string->blurb (fld columns (car pkgversions) 'pkg_blurb))
        (fld columns (car pkgversions) 'homepage)
        (let outer-loop ([rows pkgversions])
          (if (null? rows)
              '()
              (let loop ([rest (cdr rows)]
                         [first (car rows)]
                         [maj (fld columns (car rows) 'maj)]
                         [min (fld columns (car rows) 'min)]
                         [reps (list (fld columns (car rows) 'repository_id))])
                (cond
                  [(or (null? rest)
                       (not
                        (and (= maj (fld columns (car rest) 'maj))
                             (= min (fld columns (car rest) 'min)))))
                   (cons (row->pkgversion columns first reps) (outer-loop rest))]
                  [else
                   (loop
                    (cdr rest)
                    (car rest)
                    maj min
                    (cons (fld columns (car pkgversions) 'repository_id) reps))]))))
        (fld columns (car pkgversions) 'bugtrack_id))]))
  
  (define (get-package-version-by-id id user-id)
    (let* ([query (string-append
                   "SELECT * FROM all_packages WHERE package_version_id = "(number->string id)
                   " AND contributor_id = "(number->string user-id)";")]
           [resls (send *db* map query list)])
      (cond
        [(null? resls) #f]
        [else
         (let ([reps (map (λ (row) (fld (all_packages) row 'repository_id)) resls)])
           (row->pkgversion (all_packages) (car resls) reps))])))
    
  (define (reassociate-package-with-categories pkg categories)
    (let ([query
           (string-append 
            "BEGIN TRANSACTION; "
            "DELETE FROM package_categories WHERE package_id = "(number->string (package-id pkg))"; "
            (apply string-append
                   (map (λ (c)
                          (string-append
                           "INSERT INTO package_categories (package_id, category_id) "
                           "VALUES ("
                           (number->string (package-id pkg))", " 
                           (number->string (if (number? c) c (category-id c)))"); "))
                        categories))
            "COMMIT TRANSACTION;")])
      (send *db* exec query)
      (void)))
  
  (define (associate-package-with-category pkg category)
    (let ([query (string-append
                  "INSERT INTO package_categories (package_id, category_id) "
                  "VALUES ("(number->string (package-id pkg))", "
                            (if (number? category)
                                category
                                (number->string (category-id category)))")")])
      (send *db* exec query)
      (void)))
  
  (define (get-package-categories pkg)
    (let ([query (string-append
                  "SELECT c.id, c.name, c.shortname  FROM categories AS c, package_categories pc "
                  "WHERE pc.category_id = c.id "
                  " AND pc.package_id = "(number->string (package-id pkg)) "; ")])
      (send *db* map query (λ (i n s) (make-category i n s #f)))))
  
  (define (pkgversion->primary-files pv)
    (let ([query (string-append
                  "SELECT filename, interface FROM primary_files WHERE package_version_id = "(number->string (pkgversion-id pv))
                  " ORDER BY filename; ")])
      (send *db* map query (λ (f x) (make-primary-file f (if (sql-null? x) #f (read-from-string x)))))))
  
  (define (downloads-this-week pv)
    (let ([query (string-append
                  "SELECT count(*) FROM downloads "
                  "WHERE time > (current_timestamp - interval '7 days') "
                  "AND package_version_id = "(number->string (pkgversion-id pv))"; ")])
      (send *db* query-value query)))
                                        
  (define (get-all-repositories)
    (let ([query "SELECT id, name, client_lower_bound, client_upper_bound FROM repositories ORDER BY sort_order"])
      (send *db* map query make-repository)))
  
  (define (legal-repository? n)
    (and (integer? n) 
         (>= n 0)
         (let ([rn (send *db* query-value (format "SELECT count(*) FROM repositories WHERE id = ~a" n))])
           (not (= rn 0)))))
             
  ;; nat -> results
  (define (get-package-listing repository)
    (let* ([query (format "SELECT * FROM most_recent_packages WHERE repository_id = ~a" repository)]
           [rsl 
            (send *db* map query
                  (lambda row
                    (list (fld (most_recent_packages) row 'category_id)
                          (fld (most_recent_packages) row 'category_name)
                          (make-package
                           (fld (most_recent_packages) row 'package_id)
                           (fld (most_recent_packages) row 'username)
                           (fld (most_recent_packages) row 'name)
                           (blurb-string->blurb (fld (most_recent_packages) row 'pkg_blurb))
                           (fld (most_recent_packages) row 'homepage)
                           (list (row->pkgversion (most_recent_packages) row (list repository)))
                           (fld (most_recent_packages) row 'bugtrack_id)))))])
      (map
       (lambda (x) (make-category (car (car x)) (cadr (car x)) #f (cadr x))) 
       (group rsl 
             (lambda (r) (list (car r) (cadr r)))
             (lambda (r) (caddr r))))))
        
  ;; required-core-version-string->internal-code : string -> number or #f
  ;; gets an integer representing the code, or #f if the string doesn't represent
  ;; a legal core-version string.
  ;
  ;; The point of these codes is that mzscheme's version numbers aren't really numbers ---
  ;; e.g., 300.1 != 300.100 --- so I want to coerce them into numbers to compare them.
  ;; I do that by assuming that the minor number will always be at most 4 digits and
  ;; summing it with 10000*maj. Alternately I could keep two separate fields, required-core-maj
  ;; and required-core-min, but then comparison would be more awkward.
  (define (core-version-string->code rc-str)
    (let ([parsed-rc (regexp-match #px"^([0-9]+)(?:\\.([0-9]{0,4}))?$" rc-str)])
      (cond
        [(not parsed-rc) #f]
        [else
         (let ([maj (string->number (cadr parsed-rc))]
               [min (if (caddr parsed-rc)
                        (or (string->number (caddr parsed-rc)) 0)
                        0)])
           (+ (* maj 10000) min))])))
  
  (define (code->core-version-string code)
    (let ([maj (quotient code 10000)]
          [min (remainder code 10000)])
      (if (= min 0)
          (number->string maj)
          (format "~a.~a" maj min))))
  
  
  (define (add-pkgversion-to-db! user pkg maj min filename unpacked-package-path info.ss)
    ;; safe-info : symbol (-> string) [(TST -> string)] -> string
    ;; the safe version of info.ss, which escapes user-defined data but allows the
    ;; default action to produce an unescaped string. The optional final argument converts
    ;; info.ss's result into a string (which doesn't need to be escaped)
    (define safe-info
      (let ([missing (gensym)])
        (case-lambda
          [(s d) (safe-info s d (lambda (x) x))]
          [(s d c)
           (let ([ans (info.ss s (lambda () missing))])
             (cond
               [(eq? ans missing) (d)]
               [else (escape-sql-string (c ans))]))])))
    
    (let* ([rc-str (info.ss 'required-core-version (λ () #f))]
           [required-core-str 
            (cond
              [(not rc-str) "NULL"]
              [else
               (let ([required-core (core-version-string->code rc-str)])
                 (cond
                   [(not required-core) "NULL"]
                   [else (number->string required-core)]))])]
           [mainfile (info.ss 'primary-file (λ () #f))]
           [id (send *db* query-value "SELECT nextval('package_versions_pk')")]
           [query
            (string-append 
             "INSERT INTO package_versions "
             "(id, package_id, maj, min, plt_path, src_path, "
             " default_file, doctxt, release_blurb, version_name, required_core_version, downloads)"
             " VALUES "
             "("
             (number->string id) ", "
             (number->string (package-id pkg)) ", "
             (number->string maj) ", "
             (number->string min) ", "
             (escape-sql-string (path->string filename)) ", "
             (escape-sql-string (path->string unpacked-package-path)) ", "
             (safe-info 'primary-file (lambda () "NULL") (λ (x) (format "~a" x))) ", "
             (safe-info 'doc.txt (lambda () "NULL") (λ (x) (format "~a" x))) ", "
             (safe-info 'release-blurb 
                        (lambda () "NULL") 
                        (lambda (v)
                          (apply string-append (map xexpr->string (blurb->xexprs v))))) ", "
             (safe-info 'version (lambda () "NULL") (λ (x) (format "~a" x))) ", "
             required-core-str", "
             "0)")])
      (send *db* exec query)
      (when mainfile (reset-primary-files id (list (list unpacked-package-path mainfile))))
      id))
  
  ;; the association between package versions and primary files is one-to-many even
  ;; though users can only submit one primary file at the moment because users will
  ;; be able to submit more than one primary file at once in the near future
  (define (reset-primary-files id bases+files)
    (let* ([inserts
            (map (λ (base+file)
                   ;; note: we assume here that the caller has already verified that the
                   ;; package doesn't have malicious field contents; in particular that
                   ;; it doesn't have mainfiles like ../../../../etc/passwd or something
                   (let* ([basepath (car base+file)]
                          [filename (cadr base+file)]
                          [xexpr (get-interface-xexpr (build-path basepath filename))])
                     (string-append
                      "INSERT INTO primary_files (package_version_id, filename, interface) "
                      "VALUES ("
                      (number->string id)", "
                      (escape-sql-string filename)", "
                      (if xexpr (escape-sql-string (format "~s" xexpr)) "NULL")
                      "); ")))
                 bases+files)]
           [query
            (string-append
             "BEGIN TRANSACTION; "
             "DELETE FROM primary_files WHERE package_version_id = "(number->string id)"; "
             (apply string-append inserts)
             "COMMIT TRANSACTION; ")])
      (send *db* exec query)))

  ;; recompute-all-primary-files : -> void
  ;; rebuilds all package-version primary file information from the information
  ;; in source files
  (define (recompute-all-primary-files)
    (for-each-package-version
     (λ (_ pv)
       (when (and
              (pkgversion-default-file pv)
              (file-exists? (build-path (pkgversion-src-path pv) (pkgversion-default-file pv))))
         (reset-primary-files
          (pkgversion-id pv)
          (list (list (pkgversion-src-path pv) (pkgversion-default-file pv))))))))
  
  (define (sqlize-blurb xexprs)
    (if xexprs
        (escape-sql-string (blurb->blurb-string xexprs))
        "NULL"))
  
  (define (update-package-fields! pkg pkgversion blurb homepage notes default-file required-core)
    (let* ([rc-str
            (cond
              [(not required-core) "NULL"]
              [(core-version-string->code required-core) => number->string]
              [else "NULL"])]
           [pkgversion-update-statement
            (string-append 
             "UPDATE package_versions SET "
             "release_blurb = "(sqlize-blurb notes)", "
             "default_file = "(if default-file
                                  (escape-sql-string default-file)
                                  "NULL")", "
             "required_core_version = "rc-str
             " WHERE id = "(number->string (pkgversion-id pkgversion)) ";")]
          [package-update-statement
           (string-append
            "UPDATE packages SET blurb = "(sqlize-blurb blurb)", "
            " homepage = "(if homepage (escape-sql-string homepage) "NULL")
            " WHERE id = "(number->string (package-id pkg))"; ")])
      (send *db* exec pkgversion-update-statement)
      (send *db* exec package-update-statement)
      (void)))
  
  (define (associate-pkgversion-with-repository! pkgversion-id repository/repository-id)
    (let* ([rep-id (if (number? repository/repository-id)
                       repository/repository-id
                       (repository-id repository/repository-id))]
           [q (string-append
               "INSERT INTO version_repositories (package_version_id, repository_id) "
               "VALUES ("
               (number->string pkgversion-id) ", "
               (number->string rep-id) ")")])
      (send *db* exec q)
      (void)))
  
  ;; get-next-version-number : package boolean -> (cons nat nat)
  ;; gets the next version number that pkg will have, assuming that
  ;; the next version
  ;;    - is a minor update if minor-update? is true
  ;;    - is a major update is minor-update? is false
  (define (get-next-version-number pkg minor-update?)
    (let* ([query 
            (string-append
             "SELECT maj, min FROM package_versions WHERE package_id = "
             (number->string (package-id pkg))
             "ORDER BY maj DESC, min DESC LIMIT 1")]
           [maj/min-vector (send *db* query-tuple query)]) 
      (if minor-update?
          (cons-immutable (vector-ref maj/min-vector 0)
                          (add1 (vector-ref maj/min-vector 1)))
          (cons-immutable (add1 (vector-ref maj/min-vector 0))
                0))))
  
  ;; for-each-package-version : (package-stub pkgversion -> X) -> X
  ;; calls f for effect on each package version in the system [ordered username/pkgname/maj/min], and
  ;; returns the last result.
  ;; At the moment, it gives #f for pkgversion-repository to avoid having to do extra DB
  ;; transactions or software processing to batch up results. You've been warned. 
  (define (for-each-package-version fn)
    (let ([query "SELECT * FROM all_packages_without_repositories ORDER BY username, name, maj, min"])
      (send *db* for-each query
       (λ row
         (let* ([f (λ (col) (fld (all_packages_without_repositories) row col))]
                [pkg (make-package 
                      (f 'package_id)
                      (f 'username)
                      (f 'name)
                      (blurb-string->blurb (f 'pkg_blurb))
                      (f 'homepage)
                      #f
                      (f 'bugtrack_id))]
               [pkgver
                (make-pkgversion
                 (f 'package_version_id)
                 (f 'package_id)
                 (f 'maj)
                 (f 'min)
                 (string->path (f 'plt_path))
                 (string->path (f 'src_path))
                 (f 'default_file)
                 (f 'doctxt)
                 (blurb-string->blurb (f 'release_blurb))
                 (f 'version_date)
                 (f 'version_name)
                 #f
                 (let ([s (f 'required_core_version)])
                   (if s
                       (code->core-version-string s)
                       #f))
                 (f 'downloads))])
           (fn pkg pkgver))))))
                
  ;; ------------------------------------------------------------
  ;; logging
  (define (log-download ip-addr pkgver client-core-version)
    (let ([query1
           (string-append
            "INSERT INTO downloads (package_version_id, ip, client_version) "
            " VALUES ("
            (number->string (pkgversion-id pkgver))", "
            (escape-sql-string ip-addr)", "
            (escape-sql-string client-core-version)")")]
          [query2
           (string-append
            "UPDATE package_versions SET downloads = downloads + 1 WHERE id = "
            (number->string (pkgversion-id pkgver)))])
      (send *db* exec query1)
      (send *db* exec query2)
      (void)))
                                      
  (define (log-error ip-addr message)
    (let ([query
           (string-append
            "INSERT INTO errors (ip, message) "
            "VALUES ("
            (escape-sql-string ip-addr)", "
            (escape-sql-string message)")")])
      (send *db* exec query)
      (void)))
  
  ;; ============================================================
  ;; MISC
  
  (define (escape-sql-string s) 
    (sql-marshal 'varchar s))
  
  ;; group  : {listof X} (X -> (values Y Z) -> (listof (list Y (listof Z)))
  ;; groups l into contiguous stripes that give the same answer to f
  (define (group l c r)
    (if (null? l)
        '()
        (let loop ([l l]
                   [cat (c (car l))]
                   [rs-in-cat '()]
                   [other-cats '()])
          (cond
            [(null? l) 
             (reverse! (cons (list cat (reverse! rs-in-cat)) other-cats))]
            [else
             (let ([newcat (c (car l))])
               (if (equal? cat newcat)
                   (loop (cdr l)
                         cat
                         (cons (r (car l)) rs-in-cat)
                         other-cats)
                   (loop (cdr l)
                         newcat
                         (list (r (car l)))
                         (cons (list cat (reverse! rs-in-cat)) other-cats))))]))))
  
  (define (blurb->xexprs b)
    (cond
      [(string? b) (list b)]
      [(not b) (list "[no description]")]
      [else b]))
  
  ;; get-interface-expr : path[filename] -> xexpr
  ;; gets a <pre>...</pre> xexpr corresponding to the provide and provide/contract statements
  ;; in the given file, if it is a module, or a dummy otherwise.
  (define (get-interface-xexpr file)
    (let ([main-expr (with-input-from-file file read)]) ; should i check that there's nothing else following this expr?
      (cond
        [(and (pair? main-expr) (eq? (car main-expr) 'module))
         (let loop ([exprs (cdr main-expr)]
                    ;; holds a list of provide and provide/contract statements. the outer list
                    ;; is reverse sorted throughout the progream; the inner lists are verbatim
                    [provides '()])
           (cond
             [(null? exprs)
              (let* ([provides (reverse! provides)]
                     [bodies (map provide-statement->string provides)])
                `(pre ,(apply string-append bodies)))]
             [else
              (match (car exprs)
                [`(provide ,names ...)
                  (loop (cdr exprs) (cons (car exprs) provides))]
                [`(provide/contract ,clauses ...)
                  (loop (cdr exprs) (cons (car exprs) provides))]
                [_ (loop (cdr exprs) provides)])]))]
        [else
         `(pre "[non-module file]")])))

  (define (pretty-format expr)
    (parameterize ([pretty-print-columns 100])
      (let ([op (open-output-string)])
        (pretty-print expr op)
        (get-output-string op))))
  
  (define (format-provide expr)
    (string-append (pretty-format expr)))
  
  (define (format-contract expr)
    (let ([name (car expr)]
          [contract (cadr expr)])
      (string-append (format "~a ::\n" name) (pretty-format contract))))
  
  (define (provide-statement->string p)
    (match p
      [`(provide ,clause ...)
        (apply string-append (map format-provide clause))]
      [`(provide/contract ,clause ...)
        (apply string-append (map format-contract clause))]))
  
  
  )