(module display-servlet mzscheme
  
  ;; ============================================================
  ;; servlet that displays planet's contents to the world
  
  
  (require "db.ss" "data-structures.ss" "html.ss" "cookie-monster.ss" "configuration.ss")
  (require (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "url.ss" "net")
           (lib "cookie.ss" "net")
           (lib "date.ss")
           (lib "pretty.ss")
           (lib "match.ss")
           (prefix srfi1: (lib "1.ss" "srfi")))
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (startup) ; initialize the database
  
  ;; we want everything to have req in scope, so we put everything here.
  ;; alternatively we could bang a global variable, which might be kinder to memory
  (define (start req)
    
    (define-values (rep rep-explicit?)
      (apply values (request->repository req)))
        
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
      (let ([suffix (string-append "&rep=" (number->string rep))]
            [basic-operations
             (list package->link/base 
                   package->owner-link/base
                   user->link/base
                   home-link/base)])
        (apply values
               (if rep-explicit?
                   (map (lift-operation (lambda (x) (string-append x suffix)))
                        basic-operations)
                   basic-operations))))    
        
    (define (page heads bodies)
      (mkdisplay heads bodies req))
    
    ;; ============================================================
    ;; MAIN LISTING
    
    ;; generate-web-page : repository (listof xexpr[html]) (listof web-contents) -> listof xexpr[xhtml]
    ;; makes the body of a web page telling all currently-available packages
    (define (gen-main-page)
      (page
       '()
       `((div ((class "description"))
              (p (strong "PLaneT") " is PLT Scheme's centralized package distribution system. Here you "
                 "will find user-contributed Scheme packages along with instructions for using them.")
              (p "The packages on this site are user-contributed and not part of PLT Scheme. Be aware "
                 "that when you download one of them for use in your programs, you are installing "
                 "software on your computer that could deliberately or accidentally harm your system. "
                 "Do not require from PLaneT any packages you do not trust.")
              (p "For more about how to use PLaneT and for instructions on turning your own code into"
                 " packages, look up PLaneT in the DrScheme Help Desk.")
              (p "This page shows the latest versions of all available packages, sorted by category. "))
         (section "Available packages")
         (table ,@(srfi1:append-map summary-table-rows (get-package-listing rep))))))
     
    ;; ============================================================
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
               (th "Package version") (th "Version") (th "Source") (th "DLs") (th "Docs") (th "To load"))
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
      (if (pkgversion-doctxt pv)
          `("[" 
            (a ((href ,(url->string (combine-url/relative (string->url (source-code-url pkg pv)) (pkgversion-doctxt pv)))))
               "doc.txt")
            "]")
          failure))
    
    ;; file-link: package pkgversion string xexpr -> xexpr
    ;; give a link to the source code for the given file in the given package and version,
    ;; or the contents of failure if that file doesn't exist
    (define (file-link pkg pv file failure)
      (if (file-exists? (build-path (pkgversion-src-path pv) file))
         `(a ((href ,(file-url pkg pv file))) ,(format "~a" file))
         failure))
    
    ;; file-url : package pkgversion string -> string[url]
    ;; generate a url for the given file in the given package and version
    (define (file-url pkg pv file)
      (url->string (combine-url/relative (string->url (source-code-url pkg pv)) file)))
    
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
            (td ((width "*") (valign "top") (class "toload")) 
                (tt ,(to-load-fn pkg pv))))
            
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
            (get-output-string op))))
      
      (define (provide-item->table-rows p)
        (define (row* exprs) `((tr (td ,@exprs) (td nbsp))))
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
              (row* `("(all-from " ,url ,module-filename)")"))]
          [`(all-from-except ,(? string? module-filename) ,id ...)
            (let ([url (file-link pkg pv module-filename module-filename)])
              (row* `("(all-from-except " ,url ,@(space-prefix (map symbol->string id))")")))]
          [_  
           ;; (struct ...), non-string all-from and all-from-except, (all-defined), (all-defined-except ...), 
           ;; (prefix-all-defined p), and (prefix-all-defined-except ...)
           (row p)]))
      
      (define (contract->xexpr contract)
        `(code ,(pretty-format contract)))
      
      (define (provide/contract-item->table-row p)
        (define (row* id-exprs contract-exprs) `(tr (td ,@id-exprs) (td ,@contract-exprs)))
        (define (row id contract-expr) `(tr (td ,(format "~a" id)) (td ,contract-expr)))
        (match p
          [`(struct ,id-expr ((,field ,contract) ...))
            (row* `((code ,(pretty-format `(struct ,id-expr ,@field)))) `(nbsp))]
          [`(rename ,_ ,id ,contract)
            (row id (contract->xexpr contract))]
          [`(,id ,contract)
            (row id (contract->xexpr contract))]))  
      
      `(div ((class "primaryFile"))
            (div ((class "name")) 
                 ,(file-link pkg pv (primary-file-name pf) (primary-file-name pf)))
            (div ((class "interface")) 
                 ,(cond
                    [(primary-file-xexpr pf)
                     `(table
                       (thead (th "Name") (th "Contract"))
                       ,@(apply append (map provide->table-rows (primary-file-xexpr pf))))]
                    [else 
                     `(i "[no interface available]")]))))
    
    ;; gen-package-page : package -> xexpr[xhtml]
    ;; generates the web page for a particular package
    (define (gen-package-page pkg)
      (page
       (list (list (package-owner pkg) (package->owner-link pkg)) 
             (list (package-name pkg) (package->link pkg)))
       `((div ((id "packageHeader"))
               (div ((class "packageTitle")) 
                    "Package " (b ((class "packageName")) ,(package-name pkg))
                    " contributed by " (b ((class "packageOwner")) ,(package-owner pkg))
                    nbsp
                    ,@(doc-link pkg (package->current-version pkg) '())
                    ,@(if (package-homepage pkg)
                          `(nbsp "[" (a ((href ,(package-homepage pkg))) "package home page") "]")
                          '())
                    (br)
                    "To load: " (tt ,(load-current pkg (package->current-version pkg)))
                    (br)
                    "Downloads this week: " ,(number->string (downloads-this-week (package->current-version pkg)))
                    (br)
                    "Total downloads: " ,(number->string 
                                          (apply + 
                                                 (cons 
                                                  (pkgversion-downloads (package->current-version pkg))
                                                  (map pkgversion-downloads (package->old-versions pkg)))))
                    (br)
                    ) 
               (div ((class "packageBlurb"))
                    ,@(or (package-blurb pkg) '("[no description available]")))
               ,@(map 
                  (display-primary-file pkg (package->current-version pkg))
                  (pkgversion->primary-files (package->current-version pkg))))
         #;(td ((valign "top") (id "projectStats"))
               (h2 "Downloads")
               (table
                (tr
                 (td "This week:") 
                 (td ,(number->string (downloads-this-week (package->current-version pkg)))))
                (tr
                 (td "Total:") 
                 (td ,(number->string 
                       (apply + 
                              (cons 
                               (pkgversion-downloads (package->current-version pkg))
                               (map pkgversion-downloads (package->old-versions pkg)))))))))
         (section "Current version")
         ,(pvs->table pkg (list (package->current-version pkg)) load-current)
         ,@(let ([old-versions (package->old-versions pkg)])
             (if (null? old-versions)
                 '()
                 `((section "Old versions")
                   ,(pvs->table pkg old-versions load-specific)))))))
    
    ;; ============================================================
    ;; USER PAGE
    
    (define (gen-user-page user)
      (let ([pkgs (user->packages user rep)])
        (page
         (list (list (user-username user) (user->link user)))
         `((div 
            ((id "userInfo"))
            "User " (b ,(user-username user)))
           (section "Packages")
           (table ,@(map package->summary-row pkgs))))))
    
    ;; ============================================================
    ;; DISPATCHING APPARATUS
    
    (define (page-for-request initial-request)
      (with-handlers ([exn:fail? (λ (e) (default-exception-handler initial-request e))])
        (let* ([bindings (request-bindings req)]
               [pkgname  (extract-bindings 'package bindings)]
               [owner    (extract-bindings 'owner bindings)])
          (cond
            [(and (null? pkgname) (null? owner))
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
                   (let ([rpkg (filter-package-for-repository pkg rep)])
                     (if (not rpkg)
                         (page
                          (list (car owner) (car pkgname))
                          `((p "The requested package does not exist in this repository. Try another repository.")))
                         (gen-package-page pkg)))))]))))
    
    (define (default-exception-handler r e)
      (begin
        (log-error 
         (request-client-ip r)
         (format "unhandled exception: ~a" (exn-message e)))
        (mkdisplay
         '("Error")
         `((p 
            "Oops! The PLaneT server encountered an internal error and could not process your request. The error has been logged, "
            "but you may get in touch with us at planet@plt-scheme.org if you would like to tell us more about it.")
           ,@(if (DISPLAY-ERRORS-OVER-WEB?)
                 `((p "The error message was: ")
                   (pre ,(let ([op (open-output-string)])
                           (parameterize ([current-error-port op])
                             ((error-display-handler) (exn-message e) e))
                           (get-output-string op))))
                 '()))
         r)))
    
    ;; build-response : page -> response
    ;; constructs an appropriate response, given the web page to respond with
    (define (build-response v)
      (validate-xexpr v)
      (let ([new-repository (get-new-repository (request-bindings req))])
        (if new-repository
            (build-cookie-response v (list (set-cookie "rep" (number->string new-repository))))
            v)))
    
    ;; the actual executor
    (build-response (page-for-request req)))
  
  
  
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
             [(null? (cdr items)) (reverse! (cons (car items) acc))]
             [else (loop (append rsep (list (car items)) acc) (cdr items))])))]))
 
  (define (string-join sep items)
    (apply string-append (join (list sep) items))))