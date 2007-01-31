(module display-servlet mzscheme
  
  ;; ============================================================
  ;; servlet that displays planet's contents to the world
  
  
  (require "db.ss" "data-structures.ss" "html.ss" "cookie-monster.ss")
  (require (lib "servlet.ss" "web-server")
           (lib "xml.ss" "xml")
           (lib "url.ss" "net")
           (lib "cookie.ss" "net")
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
           (td ((valign "top")) ,@(package-blurb pkg))))
    
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
                ,@(if (pkgversion-doctxt pv)
                      `("[" 
                        (a ((href ,(url->string (combine-url/relative (string->url (source-code-url pkg pv)) (pkgversion-doctxt pv)))))
                           "doc.txt")
                        "]")
                      `("[none]")))
            (td ((width "*") (valign "top") (class "toload")) 
                (tt ,(to-load-fn pkg pv))))
            
        (tr (td ((colspan "4") (class "blurb"))
                ,@(or (pkgversion-blurb pv)
                      `("[no release notes]"))))))
    
    
    ;; ============================================================
    ;; PACKAGE PAGE
    
    ;; gen-package-page : package -> xexpr[xhtml]
    ;; generates the web page for a particular package
    (define (gen-package-page pkg)
      (page
       (list (list (package-owner pkg) (package->owner-link pkg)) 
             (list (package-name pkg) (package->link pkg)))
       `((div 
          ((id "packageHeader"))
          (div ((class "packageTitle")) 
               "Package " (b ((class "packageName")) ,(package-name pkg))
               " contributed by " (b ((class "packageOwner")) ,(package-owner pkg))) 
          (div ((class "packageBlurb"))
               ,@(package-blurb pkg)))
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
                       
                       (gen-package-page pkg)))))])))
    
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
    (apply string-append (join (list sep) items)))
  
  
  
  
    
  
  
  )