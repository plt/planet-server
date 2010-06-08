#lang scheme/base
(require 
 xml
 "cookie-monster.ss"
 scheme/match
 scheme/contract
 web-server/private/request-structs
 web-server/servlet/bindings
 (lib "url.ss" "net"))

;; the #:navkey arguments are all ignored here.

(require
 "configuration.ss" "data-structures.ss" "user-utilities.ss" "db.ss" "cookie-monster.ss"
 #;(file "/home/wwwplanet/svn/iplt/web/old/common/layout.ss")) ;; previous definition of 'tall-page'

(define (tall-page title #:head-stuff head-stuff #:navkey navkey . content)
  (define found-title? #f)
  (define found-body? #f)
  (begin0 (let loop ([xpr index-template])
	    (cond
	     [(and (pair? xpr) (equal? (car xpr) "{{{HEAD}}}"))
	      (append head-stuff (loop (cdr xpr)))]
	     [(equal? xpr "{{{TITLE}}}") (set! found-title? #t) title]
	     [(equal? xpr (list "{{{BODY}}}")) (set! found-body? #t) content]
	     [(pair? xpr)
	      (cons (loop (car xpr))
		    (loop (cdr xpr)))]
	     [(and (string? xpr) (regexp-match #rx"\\{\\{\\{.*\\}\\}\\}" xpr))
	      (error 'html.ss "the index-template contains this, unexpected tag ~s" xpr)]
	     [else xpr]))
	  (unless found-title? (error 'html.ss "didn't find the title tag"))
	  (unless found-body? (error 'html.ss "didn't find the body tag"))))

(define bindings/c (listof (cons/c (or/c symbol? string?) string?)))
(define title/c (or/c string? (list/c string? string?)))

(provide/contract
 [section (-> string? any)]
 [request->repository (request? . -> . (list/c natural-number/c boolean?))]
 [get-repository (bindings/c . -> . (or/c natural-number/c false/c))]
 [get-new-repository (bindings/c . -> . (or/c natural-number/c false/c))]
 [mkhtmlpage ((listof title/c) (listof any/c #|xexpr|#) . -> . any #|xexpr|#)]
 [mkdisplay (->* ((listof title/c) any/c request?)
                  (#:navkey (or/c false/c symbol?))
                  any #|xexpr|#)]
 [mkdisplay* (->* ((listof title/c) any/c natural-number/c user?)
                  ((listof (cons/c (or/c string? symbol?) string?)) 
                   #:navkey (or/c false/c symbol?))
                  any)])

(define (section str) 
  `(table ((width "100%") (cellspacing "0") (cellpadding "0")) (tr (td ((class "filledinwhite")) (b (font ((size "+1")) nbsp ,str))))))

;; ------------------------------------------------------------
;; binding stuff

;; request->repository : req -> (values number[id of a repository] boolean)
(define (request->repository req)
  (bindings->repository (request-bindings req) (request-cookies req)))

;; given a set of bindings, determines the repository to which it refers
(define (bindings->repository url-bindings cookie-bindings)
  (let ([explicit-repository-request (get-repository url-bindings)]
        [just-swapped-repository (get-new-repository url-bindings)]
        [user-setting-repository (get-repository cookie-bindings)])
    (cond
      [explicit-repository-request
       (list explicit-repository-request #t)]
      [just-swapped-repository
       (list just-swapped-repository #f)]
      [user-setting-repository
       (list user-setting-repository #f)]
      [else 
       (list (DEFAULT-REPOSITORY) #f)])))

;; get-repository : env -> number | #f
;; extracts the binding for rep in the given binding set, which must be a
;; number pointing to a repository's id
(define (get-repository bindings)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (string->number (extract-binding/single 'rep bindings))))

;; get-new-repository : bindings -> number | #f
;; extracts a repository-change request from the given bindings,
;; if it exists and is legal
(define (get-new-repository bindings)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (let* ([repstr (extract-binding/single 'changerep bindings)]
           [repnum (string->number repstr)])
      (if (and repnum (legal-repository? repnum))
          repnum
          #f))))

;; ----------------------------------------
;; HTML generation

(define (mkdisplay* titles contents rep user [bindings '()] #:navkey [navkey #f])
   (mkhtmlpage
     #:navkey navkey
     #:rss-feed (repository-id->rss-feed-url rep)
      (cons (list "PLaneT" home-link/base) titles)
      `((div ((class "nav") (align "right")) 
             (small 
              "View packages: "
              nbsp
              ,@(join
                 `(nbsp "|" nbsp)
                 (map (lambda (r)
                        (if (= (repository-id r) rep)
                            (format "[~a]" (repository-name r))
                            `(a ((href ,(repository->url-string* r bindings))) ,(repository-name r))))
                      (get-all-repositories)))
              (br)
              ,@(if user
                    `("logged in as " (b ,(user-username user)) nbsp "|" nbsp
                                      ,@(item (ADD-URL-ROOT) "manage packages") nbsp "|" nbsp
                                      ,@(item (LOGOUT-PAGE) "log out"))
                    (item (ADD-URL-ROOT) "contribute a package / log in"))))
        ,@contents)))

(define (repository-id->rss-feed-url id)
  (let ([this-one (ormap (lambda (x) (and (equal? id (repository-id x)) x))
		         (get-all-repositories))])
    (unless this-one
      (error 'repository-id->rss-feed-url "unknown id ~s" id))
    (format "/~a/planet.rss" (repository-urlname this-one))))

(define (mkdisplay titles contents req #:navkey [navkey #f])
  (let* ([rep/? (request->repository req)]
         [rep (car rep/?)])
    (mkdisplay* titles contents rep (logged-in-user req) (request-bindings req) #:navkey navkey)))

(define (item url text)
  (list "[" `(a ((href ,url)) ,@(if (string? text) (list text) text)) "]"))

(define (repository->url-string* repository old-bindings)
  (let* ([new-bindings (delete 'rep (add-or-replace 'changerep 
                                                    (number->string (repository-id repository))
                                                    old-bindings))]
         [new-url (format "~a?~a" (DISPLAY-URL-ROOT) (bindings->get-line new-bindings))])
    new-url))

(define (repository->url-string repository req)
  (repository->url-string* repository (request-bindings req)))

;; title ::= string | (list string string)
;; mkhtmlpage : (listof title) (listof xexpr[xhtml body exprs] -> xexpr[xhtml]
;;  makes an html page with the given title path and body expressions
(define (mkhtmlpage titles contents #:navkey [navkey #f] #:rss-feed [rss-feed-url #f])
  (apply
   tall-page 
   (apply string-append "PLaneT Package Repository : " (join '(" > ") (map title->string titles)))
   `(div ((class "planet"))
         (div ((class "filledinwhite"))
              (div ((class "planetNav"))
                   ,@(join '(nbsp ">" nbsp) (map title->link titles)))))
   (list `(div ((class "planet")) ,@contents))
   #:navkey navkey
   #:head-stuff `(,@(if rss-feed-url
                        (list `(link ((rel "alternate")
                                      (type "application/rss+xml")
                                      (title "RSS")
                                      (href ,rss-feed-url))))
                        '())
                  (link ((rel "stylesheet") (href "/css/main.css") (type "text/css")))
                  (link ((rel "stylesheet") (href "/css/planet-browser-styles.css") (type "text/css")))
                  (style ((type "text/css")) "import \"/css/main.css\"; import \"/css/planet-browser-styles.css\"; import \"http://www.racket-lang.org/plt.css\"; ")
                  )))

;; title->string : title -> string
(define (title->string title)
  (cond
    [(string? title) title]
    [else (car title)]))

;; title->link : title -> xexpr  
(define (title->link title)
  (cond
    [(string? title) title]
    [else `(a ((href ,(cadr title))) ,(car title))]))

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
 source-code-url/fields
 
 pkgversion->docs-link
 file-url
 
 repository->base-url
 use-full-urls?)

(define use-full-urls? (make-parameter #f))

(define (fix-url str)
  (if (use-full-urls?)
      (url->string
       (combine-url/relative
        (combine-url/relative (EXTERNAL-URL-ROOT) (URL-ROOT))
        str))
      str))

(define (package-link/fields username pkgname)
  (fix-url (format "~a?package=~a&owner=~a" (DISPLAY-URL-ROOT) pkgname username)))
(define (package->link/base pkg)
  (package-link/fields (package-owner pkg) (package-name pkg)))

(define (owner-link/fields username)
  (fix-url (format "~a?owner=~a" (DISPLAY-URL-ROOT) username)))
(define (package->owner-link/base pkg)
  (owner-link/fields (package-owner pkg)))
(define (user->link/base user)
  (owner-link/fields (user-username user)))

(define home-link/base 
  (fix-url (format "~a?" (DISPLAY-URL-ROOT))))
(define (source-code-url/fields owner name majstr minstr)
  (fix-url (string-append (WEB-PACKAGES-URL-ROOT) "/"
                          owner "/" 
                          name "/"
                          majstr "/"
                          minstr "/")))
(define (source-code-url  pkg pv)
  (source-code-url/fields (package-owner pkg)
                          (package-name pkg)
                          (number->string (pkgversion-maj pv))
                          (number->string (pkgversion-min pv))))

(define (pkgversion->docs-link pkg pv failure)
  (let ([doctxt-path (pkgversion-doctxt pv)])
    (cond
      [(not doctxt-path) (failure)]
      [else
       (let ([full-path (build-path (pkgversion-src-path pv) doctxt-path)])
         (cond
           [(file-exists? full-path)
            (file-url pkg pv doctxt-path)]
           [(and 
             (directory-exists? full-path)
             (file-exists? (build-path full-path "index.html")))
            (file-url pkg pv (path->string (build-path doctxt-path "index.html")))]
           [else (failure)]))])))

;; file-url : package pkgversion string -> string[url]
;; generate a url for the given file in the given package and version
(define (file-url pkg pv file)
  (fix-url (url->string (combine-url/relative (string->url (source-code-url pkg pv)) file))))

;; repository -> string
;; gets the base url for all static content relating to the given repository [currently just rss feed]
(define (repository->base-url rep)
  (fix-url 
   (url->string
    (combine-url/relative 
     (REPOSITORIES-STATIC-CONTENT-URL-ROOT)
     (repository-urlname rep)))))

;; mktitle : (listof string) -> string
;; makes a title with the given path
(define (mktitle strs)
  (string-join " > " (cons "PLaneT Package Repository" strs)))

(define (bindings->get-line bs)
  (string-join "&" (map (lambda (x) (format "~a=~a" (car x) (cdr x))) bs)))

(define (add-or-replace key new-val bs)
  (cond
    [(null? bs) (list (cons key new-val))]
    [(equal? (car (car bs)) key) (cons (cons key new-val) (cdr bs))]
    [else (cons (car bs) (add-or-replace key new-val (cdr bs)))]))

;; delete : symbol env -> env
;; removes all bindings for the given key in the given environment
(define (delete key bs)
  (cond
    [(null? bs) '()]
    [(eq? (car (car bs)) key) (delete key (cdr bs))]
    [else (cons (car bs) (delete key (cdr bs)))]))

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
