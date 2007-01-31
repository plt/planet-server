(module db mzscheme
  
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 1 2)))
  (require (lib "class.ss"))
  (require (lib "string.ss"))
  (require (lib "contract.ss"))
  (require (lib "xml.ss" "xml"))
  
  (require "data-structures.ss")

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
    (user? string? (or/c (listof xexpr?) false/c) . -> . package?)]
   [get-package-listing (natural-number/c . -> . (listof category?))]
   [get-matching-packages
    (string? string? string? (union natural-number/c false/c)
     natural-number/c (union natural-number/c false/c)
     . -> .
     (listof pkgversion?))]
   [get-package
    (-> string? string?
        (union package? false/c))]
   [get-package-by-id
    (-> natural-number/c natural-number/c
        (union package? false/c))]
   [get-package-version-by-id
    (-> natural-number/c natural-number/c (union pkgversion? false/c))]
   [associate-package-with-category
    (package?
     category?
     . -> .
     void?)]
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
   
   [core-version-string->code (string? . -> . (union number? false/c))]
   [code->core-version-string (number? . -> . (union string? false/c))]
   )
  
  (provide sql-null?)
  
  (define *db* #f)
  (define (startup)
    (unless *db* 
      (set! *db* (connect "localhost" 5432 "planet" "jacobm" "matrix"))
      (send *db* use-type-conversions #t)))
  (define (teardown)
    (when *db*
      (send *db* disconnect)))
  
  
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
        (make-user id username realname))))
  
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
                  " SELECT id, username, realname FROM contributors "
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
                  " SELECT id, username, realname FROM contributors "
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
                  " SELECT id, username, realname FROM contributors "
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
                            (make-package
                             (fld (packages_without_categories) row 'package_id)
                             (fld (packages_without_categories) row 'username)
                             (fld (packages_without_categories) row 'name)
                             (blurb-string->blurb (fld (packages_without_categories) row 'pkg_blurb))
                             (list (row->pkgversion (packages_without_categories) row)))))])
      results))
    
  ;; get-category-names : -> (listof category?)
  ;; produces the current list of all available categories and their corresponding ids,
  ;; sorted in presentation order
  (define (get-category-names)
    (let ([query "SELECT name, shortname, id FROM categories ORDER BY sort_order"])
      (send *db* map query (lambda (name shortname id) (make-category id name (string->symbol shortname) #f)))))
  
  ;; add-package-to-db! : user string (or (listof xexpr) #f) -> package?
  ;; adds a record for the given package information, and returns a stub
  (define (add-package-to-db! user package-name blurb)
    (let* ([id (send *db* query-value "SELECT nextval('packages_pk') AS pk")]
           [query (string-append 
                   "INSERT INTO packages (id, owner_id, name, blurb) VALUES "
                   "("(number->string id)", "
                      (number->string (user-id user))", "
                      (escape-sql-string package-name)", "
                      (if blurb 
                          (escape-sql-string (format "~s" blurb))
                          "NULL")")")])
      (begin
        (send *db* exec query)
        (make-package id (user-username user) package-name blurb '()))))
                  
  
  ;; ------------------------------------------------------------
  ;; package lookup
  
  ;; matching-packages : string string string (union nat #f) [nat (union nat #f) | if maj \in nat] -> list pkg-version
  ;; gets all the matching package versions, sorted from best to worst
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
                   " ORDER BY maj DESC, min DESC;")])
      (send *db* map query (lambda row (row->pkgversion (all_packages) row)))))
  
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
  (define (row->pkgversion columns row)
    (let* ([id (fld (all_packages) row 'package_version_id)]
           [repositories (send *db* map 
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
       (let ([notes-str (f 'release_blurb)])
         (if notes-str
             (read-from-string notes-str)
             #f))
       (f 'version_date)
       (f 'version_name)
       repositories
       (let ([s (f 'required_core_version)])
         (if s
             (code->core-version-string s)
             #f))
       (f 'downloads))))
  
  ;; get-package : string string -> (union package #f)
  (define (get-package owner name)
    (let* ([query
            (string-append
             "SELECT * FROM all_packages_without_repositories WHERE username ="(escape-sql-string owner)
             " AND name = "(escape-sql-string name)
             " ORDER BY maj DESC, min DESC")]
           [pkgversions (send *db* map query list)])
      (version-rows->package (all_packages_without_repositories) pkgversions)))
  
  ;; get-package-by-id : nat nat -> (union package #f)
  ;; gets the given package id, but only if its owner is the given owner
  (define (get-package-by-id pkg-id user-id)
    (let* ([query 
            (string-append
             "SELECT * FROM all_packages_without_repositories WHERE contributor_id = "(number->string user-id)
             " AND package_id = "(number->string pkg-id)
             " ORDER BY maj DESC, min DESC")]
           [pkgversions (send *db* map query list)])
      (version-rows->package (all_packages_without_repositories) pkgversions)))
         
  (define (version-rows->package columns pkgversions)
    (cond
      [(null? pkgversions) #f]
      [else
       (make-package
        (fld (all_packages) (car pkgversions) 'package_id)
        (fld (all_packages) (car pkgversions) 'username)
        (fld (all_packages) (car pkgversions) 'name)
        (blurb-string->blurb (fld (all_packages) (car pkgversions) 'pkg_blurb))
        (map (lambda (r) (row->pkgversion columns r)) pkgversions))]))        
  
  (define (get-package-version-by-id id user-id)
    (let* ([query (string-append
                   "SELECT * FROM all_packages WHERE package_version_id = "(number->string id)
                   " AND contributor_id = "(number->string user-id)";")]
           [resls (send *db* map query list)])
      (cond
        [(null? resls) #f]
        [(null? (cdr resls))
         (row->pkgversion (all_packages) (car resls))]
        [else
          (error 'get-package-version-by-id "too many package versions match the given id, something's wrong")])))
    
    
           
  
  (define (associate-package-with-category pkg category)
    (let ([query (string-append
                  "INSERT INTO package_categories (package_id, category_id) "
                  "VALUES ("(number->string (package-id pkg))", "
                            (number->string (category-id category))")")])
      (send *db* exec query)
      (void)))
  
  
  
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
                           (list (row->pkgversion (most_recent_packages) row))))))])
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
      id))
  
  (define (sqlize-blurb xexprs)
    (if xexprs
        (escape-sql-string (blurb->blurb-string xexprs))
        "NULL"))
  
  (define (update-package-fields! pkg pkgversion blurb notes default-file required-core)
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
            "UPDATE packages SET blurb = "(sqlize-blurb blurb)
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
    (format "'~a'" 
            (regexp-replace* #rx"'" s "\\\\'")))
  
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
      [else b])))