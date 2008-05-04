(module db mzscheme
  
  (require (planet "spgsql.ss" ("schematics" "spgsql.plt" 2 3)))
  (require (lib "class.ss"))
  (require (lib "string.ss"))
  (require (lib "contract.ss"))
  (require (lib "xml.ss" "xml"))
  (require (lib "etc.ss"))
  (require (lib "match.ss"))
  (require (lib "list.ss"))
  (require (prefix srfi13: (lib "13.ss" "srfi"))
           (prefix srfi1: (lib "1.ss" "srfi")))
  
  (require "data-structures.ss" "configuration.ss")
  
  (require scheme/path)

  ;; A note about getting packages by repository:
  ;; 
  ;; For efficiency reasons, the one-to-many mapping between package versions and repositories is not
  ;; always retrieved completely. When you get packages for a specific repository (using functions
  ;; such as get-n-most-recent-packages), the packages' package-version information will not contain
  ;; the other repositories those package versions may belong to.
  
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
   [update-user-email (user? string? . -> . void?)]
   [update-user-password (user? string? . -> . void?)]
   [user->packages (opt-> (user?) ((union (listof natural-number/c) false/c))  (listof package?))]
   [get-category-names (-> (listof category?))]
   [add-package-to-db!
    (user? string? (or/c (listof xexpr?) false/c) (or/c string? false/c) . -> . package?)]
   [get-package-listing (natural-number/c . -> . (listof category?))]
   [get-matching-packages
    (opt->*
     (string? string? string? (union natural-number/c false/c) natural-number/c (union natural-number/c false/c))
     (repository?)
     ((listof pkgversion?) boolean?))]
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
   [repository-ids->repositories (-> (listof natural-number/c) (listof repository?))]
   [legal-repository? (-> number? boolean?)]
   [legal-language? (-> string? boolean?)]
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
   [update-pkgversion-fields!
    (-> pkgversion?
        (union (listof xexpr?) false/c)
        (union string? false/c)
        (union string? false/c)
        void)]
   [associate-pkgversion-with-repository! (natural-number/c (union repository? natural-number/c) . -> . void?)]
   [get-next-version-number
    (-> package? boolean? (cons/c natural-number/c natural-number/c))]
   [get-next-version-for-maj
    (-> package? natural-number/c natural-number/c)]
   
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
   [recompute-all-primary-files (-> any)]
   [get-n-most-recent-packages (natural-number/c (union natural-number/c repository?) . -> . (listof package?))]
   
   [blurb->xexprs (any/c . -> . (union (listof xexpr?) false/c))]
    
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
        (let* (#;[start-time (current-milliseconds)]
               [conn (get-conn)])
          (dynamic-wind
           void
           (λ () (action conn))
           (λ () 
             (send conn disconnect)
             #;(timing-log query (- (current-milliseconds) start-time)))
            )))
      
      #;(define timing-log
        (let ([c (make-channel)])
          (thread 
           (λ ()
             (let loop ()
               (let-values ([(q t) (apply values (channel-get c))])
                 (with-output-to-file "/local/planet/logs/timing-log.ss"
                     (λ () (write (list q t (current-seconds))) (newline))
                     'append)
                 (loop)))))
          (λ (q t) (channel-put c (list q t)))))
        
      (define/public (query-value q . args)
        (perform-action q (λ (c) (send/apply c query-value q args))))
      (define/public (query-row q . args)
        (perform-action q (λ (c) (send/apply c query-row q args))))
      (define/public (exec q . args)
        (perform-action q (λ (c) (send/apply c exec q args))))
      (define/public (map q . args)
        (perform-action q (λ (c) (send/apply c map q args))))
      (define/public (for-each q . args)
        (perform-action q (λ (c) (send/apply c for-each q args))))
      
      (define/public (get-transaction)
        (new transaction% [conn (get-conn)]))
      
      ;; not needed if there's no connection pool
      (define/public (disconnect)
        (void))
      
      (super-new)))
  
  (define-struct (exn:fail:transaction-closed exn:fail) ())
  (define (transaction-closed!)
    (raise (make-exn:fail:transaction-closed "The transaction is closed, new commands cannot be sent to it"
                                             (current-continuation-marks))))
  
  (define transaction%
    (class* object% ()
      
      (init-field conn)
      
      (define live? #t)
      
      (send conn exec "BEGIN TRANSACTION;")
      
      (define (check-open!)
        (unless live? (transaction-closed!)))
      
      (define/public (query-value q . args)
        (check-open!)
        (send/apply conn query-value q args))
      
      (define/public (query-row q . args)
        (check-open!)
        (send/apply conn query-row q args))
      
      (define/public (exec q . args)
        (check-open!)
        (send/apply conn exec q args))
      
      (define/public (map q . args)
        (check-open!)
        (send/apply conn map q args))
      
      (define/public (for-each q . args)
        (check-open!)
        (send/apply conn for-each q args))
      
      (define/public (commit)
        (send conn exec "COMMIT TRANSACTION;")
        (send conn disconnect)
        (set! live? #f))
      
      (super-new)))
      
  (define *db*
    (new retrying-connection%
         [get-conn 
          (λ () 
            (let ([db (apply connect (DATABASE-CONNECT-ARGUMENTS))])
              db))]))
  
  ;; this should be removed eventually; it used to be useful but the functionality has been incorporated into
  ;; the retrying-connection% class.
  (define startup void)
  
  (define (teardown)
    (send *db* disconnect))
    
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
              (concat-sql
               "INSERT INTO contributors (id, username, realname, email, password) VALUES ("
               [integer id]", "
               [varchar username]", "
               [varchar realname]", "
               [varchar email]", "
               "md5("
               [varchar password]"));"))
        (make-user id username realname email))))
  
  (define (valid-password? user pass)
    (let ([query (concat-sql
                  "SELECT (password = md5("[varchar pass]")) AS valid "
                  "FROM contributors WHERE id = "[integer (user-id user)]";")])
      (send *db* query-value query)))
  
  
  (define (update-user-email user newemail)
    (let ([sql (concat-sql
                "UPDATE contributors SET email = "[varchar newemail]" "
                "WHERE id = "[integer (user-id user)]";")])
      (send *db* exec sql)
      ; i am worried that old versions of the email address might continue to float around
      ; even with this next line in place. The only fix seems to be to read these things from
      ; the database each time though
      (set-user-email! user newemail)
      (void)))
  
  ;; update-user-password : user string -> void
  (define (update-user-password user newpass)
    (let ([sql (concat-sql
                "UPDATE contributors SET password = md5("[varchar  newpass]") "
                "WHERE id = "[integer (user-id user)]";")])
      (send *db* exec sql)
      (void)))
  
  ;; gets the user record corresponding to the given username and password,
  ;; or #f if the username/password combination is invalid
  (define (get-user-record username password)
    (let ([query (concat-sql
                  " SELECT id, username, realname, email FROM contributors "
                  " WHERE username = "[varchar username]
                  " AND password = md5("[varchar password]");")])
      (let ([result (send *db* map query make-user)])
        (if (null? result)
            #f
            (car result)))))
  
  ;; get-logged-in-user-from-passcode : string? string? -> (union user? false/c)
  ;; gets the user record for a user after validating the user's passcode, a one-time
  ;; code handed out to the user after login
  (define (get-logged-in-user-from-passcode username passcode)
    (let ([query (concat-sql
                  " SELECT id, username, realname, email FROM contributors "
                  " WHERE username = "[varchar username]
                  " AND passcode = "[varchar passcode]";")])
      (let ([result (send *db* map query make-user)])
        (if (null? result)
            #f
            (car result)))))
  
  ;; log-user-in : user? -> string?
  ;; creates a passcode for the given user and returns it
  (define (log-user-in u)
    (let* ([passcode (number->string (random 999999999))]
           [query (concat-sql
                   "UPDATE contributors SET passcode = "[varchar passcode]
                   " WHERE id = "[integer (user-id u)]";")])
      (send *db* exec query)
      passcode))
  
  ;; log-user-out : user? -> void
  (define (log-user-out u)
    (let ([query (concat-sql
                  "UPDATE contributors SET passcode = NULL WHERE id = "[integer (user-id u)]";")])
      (send *db* exec query)
      (void)))
  
  ;; like get-user-record, but does not require a password. For use with
  ;; passwordless authentication (e.g., resetting password with email confirmation)
  (define (get-user-record/no-password username)
    (let ([query (concat-sql
                  " SELECT id, username, realname, email FROM contributors "
                  " WHERE username = "[varchar username]";")])
      (let ([result (send *db* map query make-user)])
        (if (null? result)
            #f
            (car result)))))
  
  ;; ------------------------------------------------------------
  ;; package insertion/management
  (define (blurb-string->blurb b)
    (if b
        (with-handlers ([exn:fail? (λ (e) #f)]) (read-from-string b))
        #f))
  
  (define (blurb->blurb-string b)
    (format "~s" b))
  
  (define user->packages
    (opt-lambda (u [repositories #f])
      (let* ([query (concat-sql "SELECT * FROM all_packages ap WHERE contributor_id = "[integer (user-id u)]
                                [#:sql
                                 (if repositories 
                                     (string-append
                                      " AND repository_id IN "
                                      "(" (srfi13:string-join (map number->string repositories) ", ") ")")
                                     "")]
                                ";")]
             [pkgversion-rows (send *db* map query list)])
        (sort 
         (version-rows->packages (all_packages) pkgversion-rows)
         (lambda (a b) (string<? (package-name a) (package-name b)))))))
            
  ;; get-category-names : -> (listof category?)
  ;; produces the current list of all available categories and their corresponding ids,
  ;; sorted in presentation order
  (define (get-category-names)
    (let ([query "SELECT name, shortname, id FROM categories ORDER BY sort_order;"])
      (send *db* map query (lambda (name shortname id) (make-category id name (string->symbol shortname) #f)))))
  
  ;; add-package-to-db! : user string (or (listof xexpr) #f) -> package?
  ;; adds a record for the given package information, and returns a stub
  (define (add-package-to-db! user package-name blurb homepage)
    (let* ([id (send *db* query-value "SELECT nextval('packages_pk') AS pk")]
           [query (concat-sql
                   "INSERT INTO packages (id, owner_id, name, blurb, bugtrack_id) VALUES "
                   "("[integer id]", "
                      [integer (user-id user)]", "
                      [varchar package-name]", "
                      [#:sql
                       (if blurb 
                           (concat-sql [varchar (format "~s" blurb)])
                           "NULL")]", "
                      "NULL" ;; dummy value because bugtracking isn't implemented yet
                      ")")])
      (begin
        (send *db* exec query)
        (make-package id (user-username user) package-name blurb homepage '() #f))))
                  
  
  ;; ------------------------------------------------------------
  ;; package lookup
  
  ;; matching-packages : string string string (union nat #f) [nat (union nat #f) | if maj \in nat] -> list pkg-version
  ;; gets all the matching package versions, sorted from best to worst
  ;; [note that ordering by hidden means that all the entries where hidden = true are after all the ones where
  ;;  it is false; this means that hidden packages are only selected if no non-hidden packages fit the request]
  (define get-matching-packages
    (opt-lambda (requester-core-version pkgowner pkgname maj minlo minhi 
                                        [repository (repository-for-version (core-version-string->code requester-core-version))])
      (let/ec return
        
        (unless repository
          (return '() #f))
      
        (let* ([rc-version (core-version-string->code requester-core-version)]
               [query1 (concat-sql
                        "SELECT * FROM all_packages "
                        " WHERE (required_core_version <= " [integer rc-version]
                        " OR required_core_version IS NULL) "
                        " AND name = "[varchar pkgname]
                        " AND username = "[varchar pkgowner]
                        [#:sql (if maj 
                                   (concat-sql " AND maj = "[integer maj]" AND min >= "[integer minlo]
                                               [#:sql
                                                (if minhi (concat-sql " AND min <= "[integer minhi]) "")])
                                   "")]
                        " AND repository_id = "[integer (repository-id repository)]
                        " ORDER BY hidden, maj DESC, min DESC;")]
               [query2 (concat-sql
                        "SELECT count(*) FROM all_packages "
                        " WHERE "
                        "  (required_core_version > "[integer rc-version]
                        "   OR repository_id <> "[integer (repository-id repository)]") "
                        " AND name = "[varchar pkgname]
                        " AND username = "[varchar pkgowner]
                        [#:sql (if maj 
                                   (concat-sql " AND maj = "[integer maj]" AND min >= "[integer minlo]
                                               [#:sql (if minhi (concat-sql " AND min <= "[integer minhi]) "")])
                                   "")]
                        "; ")]
               
               [t (send *db* get-transaction)])
          (let ([ans (send t map query1
                           (lambda row 
                             (let ([rep (fld (all_packages) row 'repository_id)])
                               (row->pkgversion (all_packages) row (list rep)))))])
            (begin0
              (cond
                [(not (null? ans)) (values ans #f)]
                [else
                 (let ([resl (send t query-value query2)])
                   (values '() (> resl 0)))])
              (send t commit)))))))
  
  ;; ----------------------------------------
  ;; table defs
  
  ;; ap-field : (listof tst)[row from all_packages view] symbol -> tst
  ;; gets the field named sym from row
  
  ;; get-column-names : symbol -> listof symbol
  ;; gets the names of all columns in the named table
  (define (get-column-names table-name)
    (let ([query (concat-sql "SELECT c.attname FROM pg_attribute c, pg_class t "
                             "WHERE c.attrelid = t.oid AND t.relname = "[varchar (symbol->string table-name)]
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
  
  ;; row->pkgversion : (listof TST)[row] (listof symbol?)[column names for the given row] (listof natural-number/c?) -> pkgversion?
  ;; converts row into a pkgversion structure.
  (define (row->pkgversion columns row repositories)
    (let* ([id (fld (all_packages) row 'package_version_id)]
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
       ;; a hack for now --- it's possible for a package to have no repositories, in which case
       ;; the natural code to write for the caller of this function will produce
       ;; (list #f) for repositories. Detect it here and fix it, though really it should be dealt
       ;; with elsewhere
       (if (equal? repositories (list #f))
           '()
           repositories)
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
              (concat-sql
               "SELECT * FROM all_packages "
               " WHERE username = "[varchar owner]
               " AND name = "[varchar name]
               [#:sql 
                (if include-hidden?
                    ""
                    " AND hidden = false")]
               " ORDER BY maj DESC, min DESC;")]
             [pkgversions (send *db* map query list)])
        (version-rows->package (all_packages) pkgversions))))
  
  ;; get-package-by-id : nat nat -> (union package #f)
  ;; gets the given package id, but only if its owner is the given owner
  (define (get-package-by-id pkg-id user-id)
    (let* ([query 
            (concat-sql
             "SELECT * FROM all_packages WHERE contributor_id = "[integer user-id]
             " AND package_id = "[integer pkg-id]
             " ORDER BY maj DESC, min DESC;")]
           [pkgversion-rows (send *db* map query list)])
      (version-rows->package (all_packages) pkgversion-rows)))
         
  (define (version-rows->packages columns pkgversion-rows)
    (map
     (λ (rows) (version-rows->package columns rows))
     (groupby (λ (pvr) (fld columns pvr 'package_id)) pkgversion-rows)))
  
  ;; this function relies on the idea the rows passed in will be from a derivative of the all_packages
  ;; query and at least have its rows as a prefix
  (define (version-rows->package columns pkgversion-rows)
    (cond
      [(null? pkgversion-rows) #f]
      [else
       (make-package
        (fld columns (car pkgversion-rows) 'package_id)
        (fld columns (car pkgversion-rows) 'username)
        (fld columns (car pkgversion-rows) 'name)
        (blurb-string->blurb (fld columns (car pkgversion-rows) 'pkg_blurb))
        (fld columns (car pkgversion-rows) 'homepage)
        (let outer-loop ([rows pkgversion-rows])
          (if (null? rows)
              '()
              (let* ([leader-row (car rows)]
                     [maj (fld columns leader-row 'maj)]
                     [min (fld columns leader-row 'min)])
                (let loop ([items (cdr rows)]
                           [reps (list (fld columns leader-row 'repository_id))])
                  (cond
                    [(or (null? items)
                         (not (and (= maj (fld columns (car items) 'maj))
                                   (= min (fld columns (car items) 'min)))))
                     (cons (row->pkgversion columns
                                            leader-row
                                            reps)
                           (outer-loop items))]
                    [else
                     (loop
                      (cdr items)
                      (cons (fld columns (car items) 'repository_id) reps))])))))
        (fld columns (car pkgversion-rows) 'bugtrack_id))]))
  
  
  ;; get-package-version-by-id : natural-number[id] natural-number[id] -> pkgversion | #f
  ;; gets the package version named by the given ids
  ;; [note: why does this need the user id, when pkgversion is more specific than user id?]
  (define (get-package-version-by-id id user-id)
    (let* ([query (concat-sql
                   "SELECT * FROM all_packages WHERE package_version_id = "[integer id]
                   " AND contributor_id = "[integer user-id]";")]
           [resls (send *db* map query list)])
      (cond
        [(null? resls) #f]
        [else
         (let ([reps (srfi1:filter-map (λ (row) (fld (all_packages) row 'repository_id)) resls)])
           (row->pkgversion (all_packages) (car resls) reps))])))
    
  (define (reassociate-package-with-categories pkg categories)
    (let ([t (send *db* get-transaction)])
      (send t exec (concat-sql "DELETE FROM package_categories WHERE package_id = "[integer (package-id pkg)]"; ")) 
      (for-each (λ (c)
                  (send t exec
                        (concat-sql
                         "INSERT INTO package_categories (package_id, category_id) "
                         "VALUES ("
                         [integer (package-id pkg)]", " 
                         [integer (if (number? c) c (category-id c))]"); ")))
                categories)
      (send t commit)))
  
  (define (associate-package-with-category pkg category)
    (let ([query (concat-sql
                  "INSERT INTO package_categories (package_id, category_id) "
                  "VALUES ("[integer (package-id pkg)]", "
                  [integer (if (number? category)
                               category
                               (category-id category))]");")])
      (send *db* exec query)
      (void)))
  
  (define (get-package-categories pkg)
    (let ([query (concat-sql
                  "SELECT c.id, c.name, c.shortname  FROM categories AS c, package_categories pc "
                  "WHERE pc.category_id = c.id "
                  " AND pc.package_id = "[integer (package-id pkg)] "; ")])
      (send *db* map query (λ (i n s) (make-category i n s #f)))))
  
  (define (pkgversion->primary-files pv)
    (let ([query (concat-sql
                  "SELECT filename, interface FROM primary_files WHERE package_version_id = "[integer (pkgversion-id pv)]
                  " ORDER BY filename; ")])
      (send *db* map query (λ (f x) (make-primary-file f (if (sql-null? x) #f (read-from-string x)))))))
  
  (define (downloads-this-week pv)
    (let ([query (concat-sql
                  "SELECT count(*) FROM downloads "
                  "WHERE time > (current_timestamp - interval '7 days') "
                  "AND package_version_id = "[integer (pkgversion-id pv)]"; ")])
      (send *db* query-value query)))
  
  
  ;; get-all-repositories : -> (listof repository?)
  ;; gets every repository in the database. Since this is assumed to change infrequently, we
  ;; cache the result. The downside is that adding a repository requires a server restart,
  ;; but that doesn't seem too terrible
  (define get-all-repositories
    (let ([all-reps #f])
      (λ ()
        (unless all-reps
          (let ([query "SELECT id, name, client_lower_bound, client_upper_bound, urlname FROM repositories ORDER BY sort_order;"])
            (set! all-reps (send *db* map query 
                                 (λ (id name lb ub url)
                                   (make-repository id name (string->number lb) (string->number ub) url))))))
        all-reps)))
  
  (define (repository-ids->repositories r-ids)
    (if (null? r-ids)
        '()
        (let ([query
               (string-append
                "SELECT id, name, client_lower_bound, client_upper_bound, urlname FROM repositories "
                "WHERE id IN ("(srfi13:string-join (map number->string r-ids) ", ")");")])
          (send *db* map query make-repository))))
  
  (define (legal-repository? n)
    (and (integer? n) 
         (>= n 0)
         (let ([rn (send *db* query-value (format "SELECT count(*) FROM repositories WHERE id = ~a" n))])
           (not (= rn 0)))))
             
  ;; nat -> results
  (define (get-package-listing repository)
    (let* ([query (format "SELECT * FROM most_recent_packages WHERE repository_id = ~a;" repository)]
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
    (cond
      ;; v200 --- v372  version numbers
      [(regexp-match #px"^([23][0-9]{2})(?:\\.([0-9]{0,4}))?$" rc-str)
       => (λ (parsed-rc)
            (let ([maj (string->number (cadr parsed-rc))]
                  [min (if (caddr parsed-rc)
                           (or (string->number (caddr parsed-rc)) 0)
                           0)])
              (+ (* maj 10000) min)))]
      ;; v3.99.0.0 and above version numbers. assume the first part is at most 2 digits long
      ;; to avoid collision with the above regexp
      [(regexp-match #px"^([1-9][0-9]?)\\.([0-9]*)(?:\\.([0-9]{1,4})(?:\\.([0-9]{1,4}))?)?$" rc-str)
       =>
       (λ (parsed-rc)
         (let-values ([(x y z w) (apply values (map (λ (x) (if x (string->number x) 0)) (cdr parsed-rc)))])
           (+ w
              (* z 100)
              (* y 10000)
              (* x 1000000))))]
      [else #f]))
  
  
  ;; repository-accepts-version? : integer[core-version code] -> repository -> boolean
  ;; determines if the given version is a core version served by the given repository
  (define ((repository-accepts-version? code) rep)
    (<= (repository-client-lower-bound rep)
        code
        (repository-client-upper-bound rep)))
  
  ;; repository-for-version : integer[core-version code] -> repository | #f
  (define (repository-for-version code)
    (let ([reps (get-all-repositories)])
      (srfi1:find (repository-accepts-version? code) reps)))
           
  (define (code->core-version-string code)
    (let ([maj (quotient code 10000)]
          [min (remainder code 10000)])
      (cond
        [(< maj 399)
         (if (= min 0)
             (number->string maj)
             (format "~a.~a" maj min))]
        [else
         (let ([x (quotient maj 100)]
               [y (remainder maj 100)]
               [z (quotient min 100)]
               [w (remainder min 100)])
           (cond
             [(and (zero? z) (zero? w))
              (format "~a.~a" x y)]
             [(zero? w)
              (format "~a.~a.~a" x y z)]
             [else
              (format "~a.~a.~a.~a" x y z w)]))])))
  
  ;; legal-language? : determine whether the database serves this particular plt scheme
  (define (legal-language? rc-str)
    (let ([code (core-version-string->code rc-str)])
      (cond
        [(not code) #f]
        [else
         (let* ([query
                 (concat-sql
                  "SELECT count(*) FROM repositories WHERE client_lower_bound <= "[integer code]
                  " AND client_upper_bound > "[integer code]";")]
                [response (send *db* query-value query)])
           (> response 0))])))
  
  ;; get-safe-info : info.ss -> symbol (-> string) [(TST -> string)] -> string
  ;; returns a 'safe' version of info.ss, which escapes user-defined data but allows the
  ;; default action to produce an unescaped string. The optional final argument converts
  ;; info.ss's result into a string (which will be escaped by this procedure)
  (define (get-safe-info info.ss)
    (let ([missing (gensym)])
      (define safe-info
        (case-lambda
          [(s d) (safe-info s d (lambda (x) x))]
          [(s d c)
           (let ([ans (info.ss s (lambda () missing))])
             (cond
               [(eq? ans missing) (d)]
               [else (concat-sql [varchar (c ans)])]))]))
      safe-info))
  
  
  (define (pred->projection pred)
    (λ (y) (if (pred y) y #f)))
  (define >file (pred->projection file-exists?))
  
  ;; html-docs-paths : path (listof string) -> (listof path)
  ;; gets all the files that Help Desk would consider valid html documentation paths
  ;; (using the criteria listed in Help Desk's documentation: either index.htm, index.html,
  ;; or <jobname>.html where <jobname>-Z-H-1.html also exists)
  (provide/contract
   [html-docs-paths (path? (listof string?) . -> . (listof path?))])
  (define (html-docs-paths root potentials)
    (parameterize ([current-directory root])
      (let ()
        (define (get-doc-path p)
          (cond
            [(>file (build-path p "index.htm"))]
            [(>file (build-path p "index.html"))]
            [else
             (with-handlers ([exn:fail? (λ (e) #f)])
               (let ([zh1s (srfi1:filter-map 
                            (λ (f) (regexp-match #rx"(.*)-Z-H-1.html$" (path->bytes f))) 
                            (directory-list (build-path root p)))])
                 (ormap 
                  (λ (x) (let ([file (build-path p (bytes->path (bytes-append (cadr x) #".html")))])
                           (>file file)))
                  zh1s)))]))
        (srfi1:filter-map get-doc-path potentials))))
    
    
    
  ;; add-pkgversion-to-db! : user package nat nat string path info.ss-fn -> nat
  ;; given information about a new package version [user, package record, maj, min, name, path to unpacked
  ;; contents, and an info.ss function] adds the new package version to the database and returns a
  ;; database primary key for that package version
  ;;
  ;; [NB user isn't currently being used here. Should it be removed?]
  ;;
  (define (add-pkgversion-to-db! user pkg maj min filename unpacked-package-path info.ss)
    (define safe-info (get-safe-info info.ss))
    (let* ([rc-str (info.ss 'required-core-version (λ () #f))]
           [required-core-str 
            (cond
              [(not rc-str) "NULL"]
              [else
               (let ([required-core (core-version-string->code rc-str)])
                 (cond
                   [(not required-core) "NULL"]
                   [else (number->string required-core)]))])]
           [main-files
            (let* ([pf-field (info.ss 'primary-file (λ () '()))]
                   [pre-main-files
                    (cond
                      [(not pf-field) '()]
                      [(string? pf-field) (list pf-field)]
                      [(list? pf-field) pf-field])])
              (filter (λ (file) (file-exists? (build-path unpacked-package-path file))) pre-main-files))]
           
           ;[html-docs-potential-paths (info.ss 'html-docs (λ () '()))]
           ;[html-docs-paths (html-docs-paths unpacked-package-path html-docs-potential-paths)]
           
           [id (send *db* query-value "SELECT nextval('package_versions_pk')")]
           [query
            (concat-sql 
             "INSERT INTO package_versions "
             "(id, package_id, maj, min, plt_path, src_path, "
             ;" default_file, doctxt, html_docs, release_blurb, version_name, required_core_version, downloads)"
             " default_file, doctxt, release_blurb, version_name, required_core_version, downloads)"
             " VALUES "
             "("
             [integer id] ", "
             [integer (package-id pkg)] ", "
             [integer maj] ", "
             [integer min] ", "
             [varchar (path->string filename)] ", "
             [varchar (path->string unpacked-package-path)] ", "
             [#:sql 
              (if (null? main-files)
                  "NULL"
                  (concat-sql [varchar (car main-files)]))]", "
             [#:sql 
              (cond
                [(info.ss 'scribblings (λ () #f))
                 =>
                 (λ (scribble)
                   (match scribble
                     [`((,(? string? file-str) (,flags ...)) ,_ ...)
                      (let* ([filename (file-name-from-path file-str)]
                             [pathname (regexp-match #rx"(.*)\\.scrbl$" (path->bytes filename))])
                        (concat-sql [varchar (path->string (build-path "planet-scribble-html" (bytes->path (cadr pathname))))]))]
                     [_ "NULL"]))]
                [(safe-info 'doc.txt (lambda () #f) (λ (x) (format "~a" x))) => (λ (x) x)]
                [else "NULL"])] ", "
             ;; note to jacob: don't check this in until you can add the column html_docs
             ;; to package_versions and rebuild all the views. PAIN!
             #|(if (null? html-docs-paths)
                 "NULL"
                 (escape-sql-string (car html-docs-paths)))", " |#
             [#:sql
              (let/ec return
                (safe-info 'release-notes 
                           (lambda () "NULL") 
                           (λ (v) 
                             (let ([xprs (blurb->xexprs v)])
                               (if xprs
                                   (format "~s" xprs)
                                   (return "NULL"))))))] ", "
             [#:sql (safe-info 'version (lambda () "NULL") (λ (x) (format "~a" x)))] ", "
             [#:sql required-core-str]", "
             "0);")])
      (send *db* exec query)
      (reset-primary-files id (map (λ (f) (list unpacked-package-path f)) main-files))
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
                          [sexpr (get-interface-sexpr (build-path basepath filename))])
                     (concat-sql
                      "INSERT INTO primary_files (package_version_id, filename, interface) "
                      "VALUES ("
                      [integer id]", "
                      [varchar filename]", "
                      [#:sql (if sexpr (concat-sql [varchar (format "~s" sexpr)]) "NULL")]
                      "); ")))
                 bases+files)]
           [t (send *db* get-transaction)])
      
      (send t exec (concat-sql "DELETE FROM primary_files WHERE package_version_id = "[integer id]"; "))
      (for-each (λ (i) (send t exec i)) inserts)
      (send t commit)))

  ;; recompute-all-primary-files : -> void
  ;; rebuilds all package-version primary file information from the information
  ;; in source files
  (define (recompute-all-primary-files)
    (for-each-package-version
     (λ (_ pv)
       (cond
         [(and
           (pkgversion-default-file pv)
           (file-exists? (build-path (pkgversion-src-path pv) (pkgversion-default-file pv))))
          (reset-primary-files
           (pkgversion-id pv)
           (list (list (pkgversion-src-path pv) (pkgversion-default-file pv))))]
         [else
         (reset-primary-files (pkgversion-id pv) '())]))))
  
  (define (sqlize-blurb xexprs)
    (if xexprs
        (concat-sql [varchar (blurb->blurb-string xexprs)])
        "NULL"))
  
  (define (update-package-fields! pkg pkgversion blurb homepage notes default-file required-core)
    (let ([t (send *db* get-transaction)])
      (update-pkg-fields/helper t pkg blurb homepage)
      (update-pv-fields/helper t pkgversion notes default-file required-core)
      (send t commit)
      (void)))
  
  (define (update-pkgversion-fields! pkgversion notes default-file required-core)
    (update-pv-fields/helper *db* pkgversion notes default-file required-core))
  
  (define (update-pkg-fields/helper conn pkg blurb homepage)
    (let ([package-update-statement
           (concat-sql
            "UPDATE packages SET blurb = "[#:sql (sqlize-blurb blurb)]", "
            " homepage = "[#:sql (if homepage (concat-sql [varchar homepage]) "NULL")]
            " WHERE id = "[integer (package-id pkg)]"; ")])
      (send conn exec package-update-statement)))
  
  (define (update-pv-fields/helper conn pkgversion notes default-file required-core)
    (let* ([rc-str
            (cond
              [(not required-core) "NULL"]
              [(core-version-string->code required-core) => number->string]
              [else "NULL"])]
           [pkgversion-update-statement
            (concat-sql
            "UPDATE package_versions SET "
            "release_blurb = "[#:sql (sqlize-blurb notes)]", "
            "default_file = "[#:sql (if default-file
                                        (concat-sql [varchar default-file])
                                        "NULL")]", "
            "required_core_version = "[#:sql rc-str]
            " WHERE id = "[integer (pkgversion-id pkgversion)] ";")])
      (send conn exec pkgversion-update-statement)))
  
  
    
  
  
  (define (associate-pkgversion-with-repository! pkgversion-id repository/repository-id)
    (let* ([rep-id (if (number? repository/repository-id)
                       repository/repository-id
                       (repository-id repository/repository-id))]
           [q (concat-sql
               "INSERT INTO version_repositories (package_version_id, repository_id) "
               "VALUES ("
               [integer pkgversion-id] ", "
               [integer rep-id] ")")])
      (send *db* exec q)
      (void)))
  
  ;; get-next-version-number : package boolean -> (cons nat nat)
  ;; gets the next version number that pkg will have, assuming that
  ;; the next version
  ;;    - is a minor update if minor-update? is true
  ;;    - is a major update is minor-update? is false
  (define (get-next-version-number pkg minor-update?)
    (let* ([query 
            (concat-sql
             "SELECT maj, min FROM package_versions WHERE package_id = "
             [integer (package-id pkg)]
             "ORDER BY maj DESC, min DESC LIMIT 1")]
           [maj/min-vector (send *db* query-row query)]) 
      (if minor-update?
          (cons (vector-ref maj/min-vector 0)
                (add1 (vector-ref maj/min-vector 1)))
          (cons (add1 (vector-ref maj/min-vector 0))
                0))))
  
  (define (get-next-version-for-maj pkg maj)
    (let* ([query 
            (concat-sql
             "SELECT MAX(min) FROM package_versions WHERE package_id = "
             [integer (package-id pkg)]
             " AND maj = " [integer maj]
             ";")]
           [the-val (send *db* query-value query)])
      (when (sql-null? the-val)
        (error 'get-next-version-for-maj "specified major version does not exist"))
      (add1 the-val)))
  
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
                 '()
                 (let ([s (f 'required_core_version)])
                   (if s
                       (code->core-version-string s)
                       #f))
                 (f 'downloads))])
           (fn pkg pkgver))))))
  
  ;; get-n-most-recent-packages : natural-number/c (union repository? natural-number/c) -> (listof package?)
  ;; gets the n most recently-updated packages in the given repository, sorted
  ;; newest-first. The pkgversions field only contains the most recent
  (define (get-n-most-recent-packages n rep)
    (let* ([rep-id (if (number? rep) rep (repository-id rep))]
            
            ;; there must be a cheaper way to do this ...
           [query (concat-sql
                   "SELECT * FROM all_packages AS ap WHERE version_date = "
                   "(SELECT max(version_date) FROM all_packages WHERE package_id = ap.package_id) "
                   " AND repository_id = "[integer rep-id]
                   " ORDER BY version_date DESC LIMIT "[#:sql (format "~a" n)]";")]
           [generate-package
            (λ row
              (let ([f (λ (n) (fld (all_packages) row n))])
                (make-package
                 (f 'package_id)
                 (f 'username)
                 (f 'name)
                 (blurb-string->blurb (f 'pkg_blurb))
                 (f 'homepage)
                 (list (row->pkgversion (all_packages) row (list rep-id)))
                 (f 'bugtrack_id))))]
           [packages (send *db* map query generate-package)])
      packages))
                
  ;; ------------------------------------------------------------
  ;; logging
  (define (log-download ip-addr pkgver client-core-version)
    (let ([query1
           (concat-sql
            "INSERT INTO downloads (package_version_id, ip, client_version) "
            " VALUES ("
            [integer (pkgversion-id pkgver)]", "
            [varchar ip-addr]", "
            [varchar client-core-version]")")]
          [query2
           (concat-sql
            "UPDATE package_versions SET downloads = downloads + 1 WHERE id = "
            [#:sql (number->string (pkgversion-id pkgver))])])
      (send *db* exec query1)
      (send *db* exec query2)
      (void)))
                                      
  (define (log-error ip-addr message)
    (let ([query
           (concat-sql
            "INSERT INTO errors (ip, message) "
            "VALUES ("
            [varchar ip-addr]", "
            [varchar message]")")])
      (send *db* exec query)
      (display message)
      (newline)
      (flush-output)
      (void)))
  
  ;; ============================================================
  ;; MISC
  
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
             (reverse (cons (list cat (reverse rs-in-cat)) other-cats))]
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
                         (cons (list cat (reverse rs-in-cat)) other-cats))))]))))
  
  (define (groupby f l)
    (let ([ht (make-hash-table)])
      (foldl
       (λ (item _)
         (let ([key (f item)])
           (hash-table-put! ht key (cons item (hash-table-get ht key (λ () '()))))))
       (void)
       l)
      (hash-table-map ht (λ (k v) v))))
                   
  
  
  (define (blurb->xexprs b)
    (cond
      [(string? b) (list b)]
      [(and (list? b) (andmap xexpr? b)) b]
      [else #f]))
  
  ;; get-interface-expr : path[filename] -> (listof sexp[provide or provide/contract clause]) or #f
  ;; gets a <pre>...</pre> xexpr corresponding to the provide and provide/contract statements
  ;; in the given file, if it is a module, or a dummy otherwise.
  (define (get-interface-sexpr file)
    (with-handlers ([exn:fail? (λ (e) #f)])
      ; should i check that there's nothing else following this expr?
      (let ([main-expr (and (file-exists? file) (with-input-from-file file read))])
        (cond
          [(not main-expr) #f]
          [(and (pair? main-expr) (eq? (car main-expr) 'module))
           (let loop ([exprs (cdr main-expr)]
                      ;; holds a list of provide and provide/contract statements. the outer list
                      ;; is reverse sorted throughout the progream; the inner lists are verbatim
                      [provides '()])
             (cond
               [(null? exprs) (reverse provides)]
               [else
                (match (car exprs)
                  [`(provide ,names ...)
                   (loop (cdr exprs) (cons (car exprs) provides))]
                  [`(provide/contract ,clauses ...)
                   (loop (cdr exprs) (cons (car exprs) provides))]
                  [_ (loop (cdr exprs) provides)])]))]
          [else #f]))))
  
  )
