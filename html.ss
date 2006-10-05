(module html mzscheme
  (require 
   "configuration.ss" "data-structures.ss"
   (file "/local/svn/iplt/web/common/layout.ss"))
  
  (provide mkhtmlpage)
  
  ;; mkhtmlpage : (listof string) (listof xexpr[xhtml body exprs] -> xexpr[xhtml]
  ;;  makes an html page with the given title path and body expressions
  (define (mkhtmlpage titles contents)
    (apply
     tall-page 
     "PLaneT Package Repository"
     #:head-stuff `(#;(link ((rel "alternate")
                           (type "application/rss+xml")
                           (title "RSS")
                           (href ,(url->string (repository->rss-url rep)))))
                    (link ((rel "stylesheet") (href "/css/main.css") (type "text/css")))
                    (link ((rel "stylesheet") (href "/css/planet-browser-styles.css") (type "text/css")))
                    (style ((type "text/css")) "import \"/css/main.css\"; import \"/css/planet-browser-styles.css\"; "))
     `((div ((class "planetNav"))
            ,@(join '(nbsp ">" nbsp) (map title->link titles)))
       ,@contents)))
         
  (define (title->link title)
    (cond
      [(string? title) title]
      [(pair? title) `(a ((href ,(cadr title))) ,(car title))]))
  
  ;; ----------------------------------------
  ;; url builders (for packages, users)
  (provide 
   package-link/fields
   package->link/base
   owner-link/fields
   package->owner-link/base
   user->link/base
   home-link/base
   
   source-code-url
   source-code-url/fields)
  
  (define (package-link/fields username pkgname)
    (format "~a?package=~a&owner=~a" (DISPLAY-URL-ROOT) pkgname username))
  (define (package->link/base pkg)
    (package-link/fields (package-owner pkg) (package-name pkg)))
  
    
  (define (owner-link/fields username)
    (format "~a?owner=~a" (DISPLAY-URL-ROOT) username))
  (define (package->owner-link/base pkg)
    (owner-link/fields (package-owner pkg)))
  (define (user->link/base user)
    (owner-link/fields (user-username user)))
  
  (define home-link/base 
    (format "~a?" (DISPLAY-URL-ROOT)))
    
  (define (source-code-url/fields owner name majstr minstr)
    (string-append (WEB-PACKAGES-URL-ROOT) "/"
                   owner "/" 
                   name "/"
                   majstr "/"
                   minstr "/"))
  
  (define (source-code-url  pkg pv)
    (source-code-url/fields (package-owner pkg)
                            (package-name pkg)
                            (number->string (pkgversion-maj pv))
                            (number->string (pkgversion-min pv))))
  
  ;; mktitle : (listof string) -> string
  ;; makes a title with the given path
  (define (mktitle strs)
    (string-join " > " (cons "PLaneT Package Repository" strs)))
  
  (define (string-join sep strs)
    (let loop ((strs strs) (acc '()))
      (cond
        [(null? strs) ""]
        [(null? (cdr strs))
         (apply string-append (reverse (cons (car strs) acc)))]
        [else
         (loop (cdr strs) (list* sep (car strs) acc))])))
  
  (define (join sep strs)
    (let loop ((strs strs) (acc '()))
      (cond
        [(null? strs) '()]
        [(null? (cdr strs))
         (apply append (reverse (cons (list (car strs)) acc)))]
        [else
         (loop (cdr strs) (list* sep (list (car strs)) acc))])))
  
  
  )
