(module display mzscheme
  
  ;; ============================================================
  ;; servlet that displays planet's contents to the world
  
  
  (require "db.ss" "data-structures.ss" "html.ss" "cookie-monster.ss" "configuration.ss")
  (require  "tracplanet/trac-admin.ss")
  (require "tracplanet/xmlrpc/xml-rpc.ss")
  (require (lib "servlet.ss" "web-server")
  (require (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "cookie.ss" "net")
           (lib "pretty.ss")
           (lib "match.ss")
           (lib "list.ss")
	   web-server/managers/none
           (prefix srfi1: (lib "1.ss" "srfi")))
  
 (define instance-expiration-handler #f)
 (define manager
        (create-none-manager instance-expiration-handler))

 
(provide interface-version timeout start)
  
  (provide gen-package-page
           req           
           rep-id
           rep-explicit?
           rep)
  
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define local-url "http://plt-scheme.org") 
  
  
  (startup) ; initialize the database
  
  (define (retract f p?)
    (λ (x) (if (p? x) (f x) #f)))
  
  (define (t-compose f g)
    (λ (x) 
      (let ([y (g x)])
        (and y (f y)))))
  
  (define (rep-id->rep id)
    (let* ([reps (get-all-repositories)]
           [therep (ormap (retract values (λ (x) (= (repository-id x) id))) reps)])
      (cond
        [(not therep)
         (error 'rep-id->name (format "no such repository: ~a" id))]
        [else 
         therep])))
  
  (define rep-id->name (t-compose repository-name rep-id->rep))
  
  
  (define (start req)
    
    (define (default-exception-handler e)
      (let ([exception-message 
             (let ([op (open-output-string)])
               (parameterize ([current-error-port op])
                 ((error-display-handler) (exn-message e) e))
               (get-output-string op))])
        (begin
          (log-error 
           (request-client-ip req)
           (format "unhandled exception: ~a" exception-message))
          (mkdisplay
           '("Error")
           `((p 
              "Oops! The PLaneT server encountered an internal error and could not process your request. The error has been logged, "
              "but you may get in touch with us at planet@plt-scheme.org if you would like to tell us more about it.")
             ,@(if (DISPLAY-ERRORS-OVER-WEB?)
                   `((p "The error message was: ")
                     (pre ,exception-message))
                   '()))
           req))))
    
    (with-handlers
        ([exn:user? (λ (e) (mkhtmlpage '("Error") `((div ((class "error")) ,(exn-message e)))))]
         [exn:fail? default-exception-handler])
      (real-start req)))
  
  (define req           (make-parameter #f))
  (define rep-id        (make-parameter #f))
  (define rep-explicit? (make-parameter #f))
  (define rep           (make-parameter #f))
  
  (define (real-start req-obj)
    
    (let* ([all-reps (get-all-repositories)]
           [repid+rep-explicit? (request->repository req-obj)]
           [rid (car repid+rep-explicit?)]
           [rexp? (cadr repid+rep-explicit?)]
           [therep (ormap (retract values (λ (x) (= (repository-id x) rid))) all-reps)])
      (cond
        [(not therep)
         (raise-user-error "You have specified a nonexistant repository.")]
        [else 
         (parameterize ([req req-obj]
                        [rep-id rid]
                        [rep-explicit? rexp?]
                        [rep therep])
           (build-response (get-page-for-request)))])))
  
  ;; ============================================================
  ;; COMMON CODE / PAGE GENERATION
  
  ;; apply op to the value induced by val (itself if it's flat, otherwise its result)
  (define ((lift-operation op) val)
    (cond
      [(procedure? val)
       (lambda args (op (apply val args)))]
      [else (op val)]))
  
  ;; if the user explicitly specified a repository in the url, then make links
  ;; carry that specification. Otherwise don't include it.
  (define-values
    (package->link package->owner-link user->link home-link)
    (let ([basic-operations
           (list package->link/base 
                 package->owner-link/base
                 user->link/base
                 home-link/base)])
      (apply values
             (if (rep-explicit?)
                 (map (lift-operation (lambda (x) (string-append x (string-append "&rep=" (number->string (rep-id))))))
                      basic-operations)
                 basic-operations))))    
  
  (define (page heads bodies)
    (mkdisplay heads bodies (req)))
  
  ;; ============================================================
  ;; MAIN LISTING
  
 
  ;; generate-web-page : repository (listof xexpr[html]) (listof web-contents) -> listof xexpr[xhtml]
  ;; makes the body of a web page telling all currently-available packages
  (define (gen-main-page)
    (page
     '()
     `((div ((class "description"))
            (table
             (tr (td ((width "60%"))
                  (p (strong "PLaneT") " is PLT Scheme's centralized package distribution system. Here you "
                     "will find user-contributed Scheme packages along with instructions for using them.")
                  (p "The packages on this site are user-contributed and not part of PLT Scheme. Be aware "
                     "that when you download one of them for use in your programs, you are installing "
                     "software on your computer that could deliberately or accidentally harm your system. "
                     "Do not require from PLaneT any packages you do not trust.")
                  (p "For more about how to use PLaneT and for instructions on turning your own code into"
                     " packages, look up PLaneT in the DrScheme Help Desk.")
                  (p "This page shows the latest versions of all available packages, sorted by category. ")
                  (p "You can be notified when new packages are added by subscribing to the "
                     (a ((href "/300/planet.rss")) "RSS feed") " or to the "
                     (a ((href "http://mailman.cs.uchicago.edu/mailman/listinfo/planet-announce")) "PLaneT-Announce mailing list")"."))
                 (td ((width "30%")(halign "RIGHT")(valign "top"))
                     (section (strong "Top Bug Closers       "))
                     (a ((href ,(string-append local-url "trac/reports/1"))) "
         [Trac]")

                     (p (table  ,@(make-bug-closer-table))))))
             (table ,@(srfi1:append-map summary-table-rows (get-package-listing
(rep-id))))))))

 
  ;; ===========================================================a
  ;; package tabling functions [utility for what follows]
  
  (define (summary-table-rows cat)
    `((tr (td ((colspan "4") (class "heading")) (h3 ,(category-name cat))))
      ,@(map package->summary-row (category-packages cat))))
  
  (define (package->summary-row pkg)
    `(tr ((bgcolor "#ddddff")) 
         (td ((valign "top")) 
             nbsp
             (a ((href ,(package->link pkg))) ,(package-name pkg)))
         (td ((valign "top"))
             ,(let* ([p (car (package-versions pkg))]
                     [version-str (or (pkgversion-name p) "")])
                (format "~a.~a" (pkgversion-maj p) (pkgversion-min p))
                )) 
         (td ((valign "top")) (a ((href ,(package->owner-link pkg))) ,(package-owner pkg)))
         (td ((valign "top")) ,@(or (package-blurb pkg) '("[no description available]")))))
  
  (define (pvs->table pkg pvs to-load-fn)
    `(table ((width "100%"))
            (thead
             (th "Package version") (th "Version") (th "Source") (th "DLs") (th "Docs") (th "Req. PLT") (th "To load"))
            ,@(srfi1:append-map (pkgversion->rows pkg to-load-fn) pvs)))
  
  (define (load-current pkg pv)
    (format "(require (planet \"~a\" (\"~a\" \"~a\" ~a ~a)))" 
            (or (pkgversion-default-file pv) "[file]")
            (package-owner pkg)
            (package-name pkg)
            (pkgversion-maj pv)
            (pkgversion-min pv)))
  
  (define (load-specific pkg pv)
    (format "(require (planet \"~a\" (\"~a\" \"~a\" ~a (= ~a))))" 
            (or (pkgversion-default-file pv) "[file]")
            (package-owner pkg)
            (package-name pkg)
            (pkgversion-maj pv)
            (pkgversion-min pv)))
  
  ;; doc-link : pkg pkgversion (listof xexpr) -> (listof xexpr)
  ;; produces a link to the documentation of the given package, given a default "no docs exist" xexpr
  (define (doc-link pkg pv failure)
    (let ([docs-link (pkgversion->docs-link pkg pv (λ () #f))])
      (if docs-link
          `("[" (a ((href ,docs-link)) "docs") "]")
          failure)))
  
  ;; file-link: package pkgversion string xexpr -> xexpr
  ;; give a link to the source code for the given file in the given package and version,
  ;; or the contents of failure if that file doesn't exist
  (define (file-link pkg pv file failure)
    (if (file-exists? (build-path (pkgversion-src-path pv) file))
        `(a ((href ,(file-url pkg pv file))) ,(format "~a" file))
        failure))
  
  ;; makes a set of table rows to present concise information about a particular version of
  ;; a given package and version.
  ;; Invariant: pv must be a version of the package pkg
  (define ((pkgversion->rows pkg to-load-fn) pv)
    `((tr (td ((width "3em") (valign "top") (class "pv"))
              (tt ,(number->string (pkgversion-maj pv)) "." ,(number->string (pkgversion-min pv))))
          ,(if (pkgversion-name pv)
               `(td ((width "5em") (valign "top") (class "v")) (b ,(pkgversion-name pv)))
               `(td ((width "5em") (valign "top") (class "v")) mdash))
          (td ((width "8em") (valign "top") (class "pv"))
              "[" (a ((href ,(source-code-url pkg pv))) "browse") "]")
          (td ((width "2em") (valign "top") (class "downloads"))
              ,(number->string (pkgversion-downloads pv)))
          (td ((width "8em") (valign "top") (class "docs"))
              ,@(doc-link pkg pv `("[none]")))
          (td ((width "8em") (valign "top") (class "pv"))
              ,(or (pkgversion-required-core pv) "[none]"))
          (td ((width "*") (valign "top") (class "toload"))
              (tt ,(to-load-fn pkg pv))))
      (tr (td ((colspan "4")) (small "Available in repositories: "
                                     ,(string-join  ", " (map rep-id->name (pkgversion-repositories pv))))))
      (tr (td ((colspan "4") (class "blurb"))
              ,@(or (pkgversion-blurb pv)
                    `("[no release notes]"))))))
  
  
  ;; ============================================================
  ;; PACKAGE PAGE
  
  (define ((display-primary-file pkg pv) pf)
    
    (define (provide->table-rows provide)
      (match provide
        [`(provide ,p ...)
         (apply append (map provide-item->table-rows p))]
        [`(provide/contract ,pc ...)
         (map provide/contract-item->table-row pc)]))
    
    (define (pretty-format i)
      (let ([op (open-output-string)])
        (parameterize ([pretty-print-columns 80])
          (pretty-print i op)
          (let* ([str (get-output-string op)]
                 [ans (regexp-match #rx"(.*)\n$" str)])
            (if ans
                (cadr ans)
                str)))))
    
    (define (provide-item->table-rows p)
      (define (row* exprs) `((tr (td ((valign "top")) (tt ,@exprs)) (td ((valign "top")) mdash))))
      (define (row i) (row* (list (pretty-format i))))
      (define (space-prefix ls)
        (cond
          [(null? ls) '()]
          [else (list* " " (car ls) (space-prefix (cdr ls)))]))
      
      (match p
        [(? symbol?)
         (row p)]
        [`(rename ,_ ,external-name)
         (row external-name)]
        [`(all-from ,(? string? module-filename))
         (let ([url (file-link pkg pv module-filename module-filename)])
           (row* `("(all-from " ,url ")")))]
        [`(all-from (file ,(? string? module-filename)))
         (let ([url (file-link pkg pv module-filename module-filename)])
           (row* `("(all-from (file \"" ,url "\"))")))]
        [`(all-from-except ,(? string? module-filename) ,id ...)
         (let ([url (file-link pkg pv module-filename module-filename)])
           (row* `("(all-from-except " ,url ,@(space-prefix (map symbol->string id))")")))]
        [`(all-from-except (file ,(? string? module-filename)) ,id ...)
         (let ([url (file-link pkg pv module-filename module-filename)])
           (row* `("(all-from-except (file \"" ,url "\" " ,@(space-prefix (map symbol->string id))")")))]
        [_  
         ;; (struct ...), non-string-or-file all-from and all-from-except, (all-defined), (all-defined-except ...), 
         ;; (prefix-all-defined p), and (prefix-all-defined-except ...)
         (row p)]))
    
    (define (contract->xexpr contract)
      `(pre ,(pretty-format contract)))
    
    (define (provide/contract-item->table-row p)
      (define (row* id-exprs contract-exprs) `(tr (td ((valign "top")) (tt ,@id-exprs)) (td ((valign "top")) ,@contract-exprs)))
      (define (row id contract-expr) `(tr (td ((valign "top")) (tt ,(format "~a" id))) (td ((valign "top")) ,contract-expr)))
      (match p
        [`(struct ,id-expr ((,field ,contract) ...))
         (row* `((code ,(pretty-format `(struct ,id-expr ,@field)))) `(nbsp))]
        [`(rename ,_ ,id ,contract)
         (row id (contract->xexpr contract))]
        [`(,id ,contract)
         (row id (contract->xexpr contract))]))
    
    `(div ((class "primaryFile"))
          (div ((class "name")) ,(file-link pkg pv (primary-file-name pf) (primary-file-name pf)))
          (div ((class "interface")) 
               ,(cond
                  [(primary-file-xexpr pf)
                   `(table ((class "interface"))
                           (tr (td (b "Name")) (td (b "Contract")))
                           ,@(apply append (map provide->table-rows (primary-file-xexpr pf))))]
                  [else 
                   `(i "[no interface available]")]))))
  
 (define (tickets-open list-of-ids)
     (filter (lambda(x)
                (not (string=? "closed" (ticket-status (ticket-get-wrapper x))))) list-of-ids))
  ;; gen-package-page : package -> xexpr[xhtml]
  ;; generates the web page for a particular package
  (define (gen-package-page pkg)
    (let*-values ([(all-reps) (get-all-repositories)]
                  [(all-packages) (package-versions pkg)]
                  
                  ;; invariant: (cons? (append available unavailable))
                  [(available unavailable) 
                   (srfi1:partition (λ (pv) (memq (rep-id) (pkgversion-repositories pv)))
                                    all-packages)]
                  [(current) (if (null? available)
                                 (car unavailable)
                                 (car available))]
		  [(tq)  (ticket-query (string-append "component=" (package-name pkg)))]
		  [(ttq (tickets-open tq)])
      (page
       (list (list (package-owner pkg) (package->owner-link pkg)) 
             (list (package-name pkg) (package->link pkg)))
       `(,@(if (null? available)
               `((div ((class "warning")) 
                      ,(format "This package is not available in the ~a repository." (repository-name (rep)))
                      " Showing package versions available for all repositories instead."))
               '())
         (div ((id "packageHeader"))
              (div ((class "packageTitle")) 
                   "Package " (b ((class "packageName")) ,(package-name pkg))
                   " contributed by " (b ((class "packageOwner")) ,(package-owner pkg))
                   nbsp
                   ,@(doc-link pkg current '())
                   ,@(if (package-homepage pkg)
                         `(nbsp "[" (a ((href ,(package-homepage pkg))) "package home page") "]")
                         '())
                   (table ((width "95%"))
                          (tr
                           (td ((width "18%")) "To load: ")
                           (td (tt ,(load-current pkg current))))
                          ,@(if (pkgversion-required-core current)
                                `((tr
                                   (td "Required PLT Scheme version: ")
                                   (td (tt ,(pkgversion-required-core current)))))
                                '())
                          (tr 
                           (td ((valign "top")) "Package description: ")
                           (td ((class "packageBlurb"))
                               ,@(or (package-blurb pkg) '("[no description available]"))))
                          (tr
                           (td "Downloads this week: ")
                           (td ,(number->string (downloads-this-week current))))
                          (tr
                           (td "Total downloads: ")
                           (td ,(number->string (foldl (λ (t r) (+ (pkgversion-downloads t) r)) 0 (package-versions pkg)))))
                          (tr
		           (td "Number of bug reports:")
                           (td ,(number->string (length tq))))
                          (tr
                           (td "Number of open bug reports:")
                           (td ,(number->string (length ttq))))
	           	  (tr
                           (td ((valign "top")) "Primary files: ")
                           (td ,@(map 
                                  (display-primary-file pkg current)
                                  (sort
                                   (pkgversion->primary-files current)
                                   (λ (a b) (string<? (primary-file-name a) (primary-file-name b))))))))))
         ,@(if (null? available)
               '()
               `((section "Current version")
                 ,(pvs->table pkg (list (car available)) load-current)
		,@(if (null?  tq)
                       `((section "No Tickets Currently open for this Package"))                       `((section "Open Tickets available for this Package")
                         "["
                         (a ((href ,(string-append
                                     local-url
                                     "trac/query?component="
                                     (package-name pkg))))
                            "All Tickets")
                         "]"
                         ,(package->bug-table (package-name pkg))))
                 ,@(let ([old-versions (cdr available)])
                     (if (null? old-versions)
                         '()
                         `((section "Old versions")
                           ,(pvs->table pkg old-versions load-specific))))))
         ,@(if (null? unavailable)
               `()
               `((section "Packages in other repositories")
                 (p ,(format "These packages are not available in the ~a repository, but they are available for other versions of PLT Scheme." 
                             (repository-name (rep))))
                 ,(pvs->table pkg unavailable load-specific)))))))
  ;; ============================================================
 ;Functions for creating bug table on a page

  ; string -> (listof xexpr?)
  (define (package->bug-table component)
    (table-with-component-fields (query->table "component" component)))

  ;string? string? -> (listof ticket?)
  (define (query->table type selector)
    (filter (lambda (x)
              (not (equal?
                    (ticket-status x)
                    "closed")))
            (map (lambda(x) (ticket-get-wrapper x))
                 (ticket-query (string-append type "="
                                              selector)))))


  ;(listof ticket?)-> (listof xexpr)
  (define (table-with-component-fields tickets)
    (table-with-fields (list "Ticket Id" "Owner" "Reporter" "Type" "Version") tickets ticket->row))

   ;(listof ticket?)-> (listof xexpr)
  (define (table-with-owner-fields tickets)
    (table-with-fields (list "Ticket Id" "Component" "Reporter" "Type" "Version") tickets ticket->owner-row))

  ;(listof string? ) (listof ticket?) (-> ticket? (listof xexpr?))-> (listof xexpr)
  (define (table-with-fields list-of-fields tickets row-function)
    (append
        (if (null? tickets)
                `(section "This user has no tickets.")
                `(table ((width "100%"))
                        (tr (b
                          ,@(map (lambda(x) `(td ,x)) list-of-fields)))
                                ,@(srfi1:append-map row-function tickets)))
                `((a ((href ,(string-append local-url "trac/newticket")))) "[New Ticket]")))


  (define (ticket->gen-row t second-field)
    `((tr ((bgcolor "#ddddff"))
          (td ((valign "center") (halign "center") (class "Ticket"))
              (a ((href ,(string-append
                          local-url
                          "trac/ticket/"
                          (ticket-id t))))) ,(ticket-id t))
          ,@(cond [(equal? second-field ticket-component)
                   `((td ((valign "center") (class "owner"))
                        (a ((href ,(string-append
                                    local-url
                                    "display.ss?package="
                                    (ticket-component t)
                                    "&owner="
                                    (ticket-owner t)
                                    ))))
                        ,(ticket-component t)))]
                  [(equal? second-field ticket-owner)
                   `((td ((valign "center") (class "owner"))
                        (a ((href ,(string-append
                                    local-url
                                    "display.ss?owner="
                                    (ticket-owner t) )))),(ticket-owner t)))]
                  [else `((td ((valign "center") (class "reporter")) ,(second-field t)))])
          (td ((valign "center")) ,(ticket-reporter t))
          (td ((valign "center")) ,(ticket-type t))
          (td ((valign "center")) ,(ticket-version t)))
      (tr (td ((valign "top")) ,(ticket-summary t)))))
   ;ticket -> (listof xexpr)
  (define (ticket->owner-row t)
    (ticket->gen-row t ticket-component))




  ;ticket->listof[xepr]
  ;takes a ticket and turns it into html as the first item in a list
  (define (ticket->row  ticketa)
    (ticket->gen-row ticketa ticket-owner))


  ; list? exact-nonneg-int? -> (and/c list? (=? (length list) exact-nonneg int))
  (define (take-it lista number)
    (if (or (zero? number) (empty? lista))
        empty
        (cons (first lista) (take-it (rest lista) (- number 1)))))

  ;bug-closer-table: void -> listof xexpr?[tr]
  (define (bug-closer-rows)
    (let* ([query-results (ticket-query "status=closed")]
           [bug-closers (map ticket-owner (map ticket-get-wrapper  query-results))]
           [hash-table (make-hash-table 'equal)])
      (for-each (lambda(x)
                  (let* ([value (hash-table-get hash-table x 0)])
                    (hash-table-put! hash-table x (+ 1 value))))
                bug-closers)
      (let* ([hash-list (hash-table-map hash-table
                                        (lambda(x y)
                                          (cons y x)))]
             [sorted           (sort hash-list (lambda(x y)
                                                 (> (first x) (first y))))]
             [top-three         (take-it sorted (min (length sorted) 3))])
        (if (null? top-three)
            (list `(tr (td ((colspan "2"))
                           (i "There are no current closed bug reports. Be the first to close a bug!"))))
            (srfi1:append-map (lambda(x) (fill-bug-table
                                          (rest x)
                                          (first x)))
                              top-three)))))


  (define (fill-bug-table name bugs)
    `((tr ((bgcolor "#ddddff"))
          (td ((colspan "40") (valign "top") (class "User")) ,name)
          (td ((colspan "40") (valign "center") (class "Bugs")) ,(number->string bugs)))))

  (define (make-bug-closer-table)
    `((tr (td ((class "heading")) (strong "Username"))
          (td ((class "heading")) (strong "# Closed")))
      ,@(bug-closer-rows)))


 
  ;; ============================================================
  ;; USER PAGE
  
  (define (gen-user-page user)
    (let ([pkgs (user->packages user (list (rep-id)))])
      (page
       (list (list (user-username user) (user->link user)))
       `((div 
          ((id "userInfo"))
          "User " (b ,(user-username user)))
         (section "Packages")
         (table ,@(map package->summary-row pkgs))
	(section "Open Tickets Owned by User")
        ,(table-with-owner-fields (query->table "owner" (user-username user)))))))
	 
  
  ;; ============================================================
  ;; DISPATCHING APPARATUS
  
  ;; the page to show
  (define (get-page-for-request)
    (let* ([bindings (request-bindings (req))]
           [pkgname  (extract-bindings 'package bindings)]
           [owner    (extract-bindings 'owner bindings)])
      (cond
        [(null? owner)
         ;; ideally there would be a separate page saying you can't do that, but for now I just 
         ;; want to stop the internal error
         (gen-main-page)]
        [(and (null? pkgname) (not (null? owner)))
         (let ([user (get-user-record/no-password (car owner))])
           (if (not user)
               (page
                (list (car owner))
                `((p "The requested user does not exist.")))
               (gen-user-page user)))]
        [else
         (let ([pkg (get-package (car owner) (car pkgname))])
           (if (not pkg)
               (page
                (list (car owner) (car pkgname))
                `((p "The requested package does not exist.")))
               (gen-package-page pkg)))])))
  
  ;; build-response : page -> response
  ;; constructs an appropriate response, given the web page to respond with
  (define (build-response v)
    (validate-xexpr v)
    (let ([new-repository (get-new-repository (request-bindings (req)))])
      (if new-repository
          (build-cookie-response v (list (set-cookie "rep" (number->string new-repository))))
          v)))
  
  
  
  ;; ============================================================
  ;; helpers
  ;; [these are available in scheme/list or something similar now]
  
  
  ; join : (listof x) (listof x) -> listof x
  ; intersperses all values in separator between each element of items
  (define (join separator items)
    (cond
      [(null? items) '()]
      [else
       (let ([rsep (reverse separator)])
         (let loop ([acc '()]
                    [items items])
           (cond
             [(null? (cdr items)) (reverse (cons (car items) acc))]
             [else (loop (append rsep (list (car items)) acc) (cdr items))])))]))
  
  (define (string-join sep items)
    (apply string-append (join (list sep) items))))
