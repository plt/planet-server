(module repository mzscheme
  
  (provide (all-defined))
  
  (require "archives.ss"
           "server-config.ss"
           
           "email.ss"
           "repository-types.ss"
           
           (lib "planet-shared.ss" "planet" "private")
           (lib "list.ss")
           (lib "file.ss")
           (lib "pregexp.ss")
           (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "date.ss")
           (lib "match.ss")
           
           (prefix srfi1: (lib "1.ss" "srfi"))
           (prefix srfi13: (lib "13.ss" "srfi"))
           
           (file "../web/common/layout.ss"))
  
  ;; ============================================================
  ;; TYPES
  ;; The repository code deals centrally with these structures
  ;; and values
  
  (define-struct cache
    (hash  ; hash-table[(string . string) -o> (listof package-descriptor)]
     file  ; path[absolute,filename]
     pkg-list ; listof installed-pkg (ordered by arrival, newest first. no outdated versions)
     )
    (make-inspector))
  
  ;; add-archive-to-repository : string path[file] bool [(listof string)] -> void
  ;; adds the given package to the repository.
  ;; owner    : the owner of the package
  ;; plt-file : the file containing the package
  ;; bool     : #t if this is a minor update, #f for major
  ;; version  : the version the package was written for. Defaults to the server's version.
  (define add-archive-to-repository
    (case-lambda 
      [(owner plt-file minor-update?) 
       (add-archive-to-repository owner plt-file minor-update? (list (version)))]
      [(owner plt-file minor-update? langs)
       (let ((installations
              (map 
               (lambda (lang) (additize owner plt-file minor-update? lang))
               langs)))
         (announce-new-package installations))]))

  ;; additize : string path[file] bool string -> installed-package
  ;; add a package to a particular repository
  (define (additize owner plt-file minor-update? lang)
    (define repository (get-repository lang))
    (define package-name (path->string (file-name-from-path plt-file)))
    (let*-values ([(latest) (get-latest-version (build-key owner package-name) repository)]
                  [(new-maj new-min)
                   (cond
                     [(not latest)  (values 1 0)]
                     [minor-update? (values (package-maj latest)
                                            (add1 (package-min latest)))]
                     [else          (values (add1 (package-maj latest))
                                            0)])]
                  [(pkg)
                   (make-package
                    package-name
                    lang
                    owner
                    new-maj
                    new-min)])

      (install-package pkg plt-file repository)))

  ;; announce-new-package : (listof installed-package) -> void
  ;; takes care of announcing the new package to the world
  (define (announce-new-package installations)
    
    ;; this should defined above, but until the web stuff gets moved back into
    ;; its own file I can't do that
    (define NOTIFIERS
      (list update-web-site update-rss-feed #;update-email-list))
    
    (for-each (lambda (n) (n installations)) NOTIFIERS))
  
  ;; ============================================================
  ;; REPOSITORY

  ;; get-repository : string -> repository
  ;; returns the repository that handles lang
  (define get-repository
    (let ((rep-ht (make-hash-table)))
      (lambda (lang)
        (let ([repository-name
               (or 
                (language-version->repository lang)
                (raise (make-exn:fail 
                        (format "No repository exists for language" lang)
                        (current-continuation-marks))))])
          (hash-table-get 
           rep-ht
           repository-name
           (lambda ()               
             (let ([rep (make-repository repository-name (build-path repository-name) #f)])
               (set-repository-cache! 
                rep
                (begin
                  (unless (file-exists? (repository->cache-file rep))
                    (build-cache rep))
                  (read-cache rep)))
               (hash-table-put! rep-ht repository-name rep)
               rep)))))))
  
  ;; repository->cache-file : repository -> path[absolute,file]
  ;; gives the name of a file where a cache should be stored for this repository
  (define (repository->cache-file rep)
    (build-path (PLANET-SERVER-REPOSITORY) (repository-directory rep) "cache.ss"))
  
  ;; install-package : package path repository -> installed-package
  ;; puts the package in the given repository
  (define (install-package pkg pkg-file repository)
    (let* ([relative-pkg-path (pkg->relative-dir-path pkg)]
           [pkg-path (build-path (PLANET-SERVER-REPOSITORY) (repository-directory repository) relative-pkg-path)]
           [new-pkg-name (build-path pkg-path (package-name pkg))])
      (make-directory* pkg-path)
      (copy-file pkg-file new-pkg-name)
      (extract-files-from-archive new-pkg-name pkg-path (pregexp-quote "./info.ss"))
      (let ((installed-pkg 
             (make-installed-package
              (package-name pkg)
              (package-lang pkg)
              (package-owner pkg)
              (package-maj pkg)
              (package-min pkg)
              (current-seconds)
              repository
              #f)))
        (add-to-cache (repository-cache repository) installed-pkg)
        installed-pkg)))
  
  ;; get-latest-version : key repository -> package-descriptor | #f
  ;; given a key, returns the most recent package that matches it
  (define (get-latest-version package-key repository)
    (let ([versions (cache-get (repository-cache repository) package-key)])
      (if versions
          (latest-version-from-list versions)
          #f)))
  
  ; get-all-repositories : -> (listof repository)
  ; this is a hack
  (define (get-all-repositories)
    (srfi1:filter-map 
     (lambda (rpath) 
       (let ((r (path->string rpath)))
         (if (string->number r)
             (get-repository r)
             #f)))
     (directory-list (PLANET-SERVER-REPOSITORY))))
  
  
  ;; ----------------------------------------
  ;; cache
  
  ; build-cache : repository -> cache
  ; side-effect : builds a repository-cache from the given repository directory
  ; and saves it in path
  (define (build-cache rep)
    (define file (repository->cache-file rep))
    (define path (path-only file))
    (define (ok? . p)
      (lambda (file) (directory-exists? (apply build-path `(,path ,@p ,file)))))
    
    (define ht (make-hash-table 'equal))
    
    (comp ((owner   (filter (ok?)                   (directory-list path)))
           (package (filter (ok? owner)             (directory-list (build-path path owner))))
           (maj     (filter (ok? owner package)     (directory-list (build-path path owner package))))
           (min     (filter (ok? owner package maj) (directory-list (build-path path owner package maj)))))
          (add-to-cache-ht 
           ht
           (make-installed-package 
            (path->string package)
            (repository-name rep)
            (path->string owner)
            (string->number (path->string maj))
            (string->number (path->string min))
            (file-or-directory-modify-seconds (build-path path owner package maj min))
            rep
            #f)))
    
    (let ((c (make-cache 
              ht
              file
              (srfi1:delete-duplicates
               (quicksort (apply append (hash-table-map ht (lambda (k v) v))) pkg<)
               (lambda (a b) (equal? (key a) (key b)))))))
      (write-cache c)
      c))
  
  (define (read-cache rep)
    (let* ((cache-file (repository->cache-file rep))
           (marshalled-cache (with-input-from-file cache-file read))
           (ht (make-hash-table 'equal))
           (l (map (λ (m) (readable->installed-package m rep)) marshalled-cache)))
      (for-each (λ (i) (add-to-cache-ht ht i)) l)
      (make-cache ht cache-file l)))
  
  ; key : package-descriptor -> package-key
  ; extracts a key for this package suitable for use with repository caches.
  ; all packages with the same owner and package name share a key.
  (define (key d) (cons (package-owner d) (package-name d)))
  
  ; build-key : string string -> package-key
  (define build-key cons)
  
  ; cache-get : cache package-key -> (listof package-descriptor) | #f
  (define (cache-get cache k)
    (hash-table-get (cache-hash cache) k (lambda () #f)))
  
  ; write-cache : cache -> void
  ; writes out the given cache.
  (define (write-cache c)
    (with-output-to-file (cache-file c)
      (lambda () 
        (write 
         (map 
          installed-package->readable
          (cache-pkg-list c))))
      'replace))
  
  ; add-to-cache-ht : hash-table package -> void
  ; adds pkg to the given hash table
  (define (add-to-cache-ht ht pkg)
    (hash-table-put! 
     ht
     (key pkg)
     (cons pkg (hash-table-get ht (key pkg) (lambda () '())))))
  
  ;; add-to-cache : cache package-descriptor -> void
  ;; adds the given package to the cache (does not add to the repository)
  (define (add-to-cache cache pkg)
    (add-to-cache-ht (cache-hash cache) pkg)
    (set-cache-pkg-list!
     cache 
     (insert pkg (srfi1:delete pkg (cache-pkg-list cache) (lambda (a b) (equal? (key a) (key b)))) pkg<))
    (write-cache cache))
  
  ;; ============================================================
  ;; PACKAGE UTILITY
  
  ; extract-package-file : package-descriptor repository string path -> boolean
  ; copies the file named filename from the given package (named by descriptor and
  ; repository) into the given path. Returns whether it was successful.
  (define (extract-package-file pkg filename path)
    (extract-files-from-archive (pkg->absolute-file-path pkg) path (pregexp-quote filename))
    (if (directory-exists? path)
        (file-exists? (build-path path filename))
        (file-exists? filename)))
  
  ;; latest-version-from-list : (listof package) -> package
  ;; given a list of packages, returns the highest version
  (define (latest-version-from-list versions)
    (extract-max 
     versions
     (lambda (a b) 
       (or (< (package-maj a) (package-maj b))
           (and (= (package-maj a) (package-maj b))
                (< (package-min a) (package-min b)))))))
  
  ;; ============================================================
  ;; RSS
  
  ;; repository -> url
  (define (repository->rss-url rep)
    (combine-url/relative (repository->url rep) (RSS-FILE-NAME)))
  
  ;; update-rss-feed : notifier
  (define (update-rss-feed pkgs)
    (for-each 
     (lambda (pkg) (rebuild-rss-feed (installed-package-repository pkg)))
     pkgs))
  
  (define (rebuild-rss-feed rep)
    (let* ([new-rss-xexpr
            `(rss 
              ((version "2.0"))
              (channel
               (title "New PLaneT Packages")
               (link ,(url->string (repository->url rep)))
               (description "The newest packages available from PLaneT")
               (language "en-us")
               ,@(map
                  (lambda (pkg)
                    `(item (title ,(package-name pkg))
                           (description 
                            ,(let ((op (open-output-string)))
                               (for-each
                                (lambda (x) (write-xml/content (xexpr->xml x) op))
                                (pkg->blurb pkg))
                               (get-output-string op)))
                           (link ,(url->string (pkg->url pkg)))))
                  (take-up-to (cache-pkg-list (repository-cache rep)) (NUM-RSS-ITEMS)))))]
           [file-to-write 
            (build-path (WEBROOT) (repository-name rep) (RSS-FILE-NAME))]
           [o 
            (open-output-file file-to-write 'replace)])
      (display "<?xml version=\"1.0\"?>\n" o)
      (write-xml/content (xexpr->xml new-rss-xexpr) o)
      (close-output-port o)))
  
  ;; ============================================================
  ;; WEB SITE
  
  ;; legal categories
  ;; uncategorized packages will be given the last category in the
  ;; table, and the web page is sorted in table order
  (define category-table
    '((devtools       "Development Tools")
      (net            "Networking and Protocols")
      (media          "Graphics and Audio")
      (xml            "XML-Related")
      (datastructures "Data Structures and Algorithms")
      (io             "Input/Output and Filesystem")
      (scientific     "Mathematical and Scientific")
      (system         "Hardware/Operating System-Specific Tools")
      (ui             "Textual and Graphical User Interface")
      (planet         "PLaneT-Related")
      (misc           "Miscellaneous")))

  (define categories (map car category-table))
  
  (define (cat< a b)
    (unless (memq a categories)
      (error 'cat< "not a legal category: ~s" a))
    (memq b (memq a categories)))

  (define (cat->string c)
    (let ((cp (assq c category-table)))
      (unless cp
        (error 'cat->string "not a legal category: ~s" c))
      (cadr cp)))
  
  (define (symbol->category s)
    (let ((a (assq s category-table)))
      (and a (car a))))
  
  ;; package->categories : installed-package -> (listof symbol)
  ;; gets the categories in which to file this package
  (define (package->categories p)
    (let ((c-symbols ((installed-package->info p) 'categories #f)))
      (match c-symbols
        [((? symbol?) ...)
         (let ((ans (srfi1:filter-map symbol->category c-symbols)))
           (or ans (caar (last-pair category-table))))]
        [_
         (list (caar (last-pair category-table)))])))
            
  (define (rebuild-web-site)
    (for-each 
     (lambda (r)
       (rebuild-repository-pages r)
       (rebuild-rss-feed r))
     (get-all-repositories)))
  
  
  ;; web-contents : installed-package | (cons string (listof web-contents))
  
  ;; update-web-site : notifier
  (define (update-web-site pkgs)
    
    (define (install-doc-file pkg doc-dir)
      (let ((doc-file "doc.txt"))
        (make-directory* doc-dir)
        (extract-package-file pkg (pregexp-quote doc-file) doc-dir)))
    
    (define (update-web-site/internal pkg)
      (let ((doc-dir (pkg->doc-dir pkg)))
        (cond
          [((installed-package->info pkg) 'doc.txt #f)
           =>
           (lambda (doc-file)
             (when (equal? doc-file "doc.txt")
               (install-doc-file pkg doc-dir)))]
          
          ;; special case for v20x packages; try to add doc.txt always
          [(string=? (repository-name (installed-package-repository pkg)) "20x")
           (install-doc-file pkg doc-dir)]

          [else (void)])
        (rebuild-repository-pages (installed-package-repository pkg))))
    
    (for-each update-web-site/internal pkgs))
  
  (define (rebuild-repository-pages rep)
    (build-web-page rep)
    (build-new-additions-page rep))
  
  ;; packages-by-category : repository -> (listof (list symbol package ...))
  (define (packages-by-category rep)
    (let ((ht (make-hash-table)))
      (for-each
       (lambda (p)
         (for-each 
          (lambda (cat) 
            (hash-table-put! 
             ht
             cat
             (cons p (hash-table-get ht cat (lambda () '())))))
          (package->categories p)))
       (hash-table-map (cache-hash (repository-cache rep)) (lambda (k v) (latest-version-from-list v))))
      (hash-table-map ht cons)))
  
  ; repository->web-contents : repository -> listof web-contents
  ; gets a tree from the repository suitable for marshalling into a web page
  (define (repository->normal-page rep)
    (let* ([the-categories
            (packages-by-category rep)])
      (map 
        (lambda (category-stuff)
          (cons (cat->string (car category-stuff))
                (quicksort 
                 (cdr category-stuff)
                 (lambda (a b) (string<? (package-name a) (package-name b))))))
        (quicksort 
         the-categories
         (lambda (a b) (cat< (car a) (car b)))))))
  
  (define PKGS-PER-PAGE 50)
  
  ;; repository->newest-first-page : repository -> listof web-contents
  ;; gets a list of the newest packages
  (define (repository->newest-first-page rep)
    (take-up-to (cache-pkg-list (repository-cache rep)) PKGS-PER-PAGE))
  
  ;; build-web-page : repository -> void
  ;; builds the web page for the given repository
  (define (build-web-page rep)
    (build-web-page* 
     (WEB-PAGE-FILE)
     rep 
     `((p (strong "PLaneT") " is PLT Scheme's centralized package distribution system. Here you "
          "will find user-contributed Scheme packages along with instructions for using them.")
       (p "The packages on this site are user-contributed and not part of PLT Scheme. Be aware "
          "that when you download one of them for use in your programs, you are installing "
          "software on your computer that could deliberately or accidentally harm your system. "
          "Do not require from PLaneT any packages you do not trust.")
       (p "For more about how to use PLaneT and for instructions on turning your own code into"
          " packages, look up PLaneT in the DrScheme Help Desk.")
       (p "This page shows the latest versions of all available packages, sorted by category. "))
     (repository->normal-page rep)))
  
  ;; build-new-additions-page : repository -> void
  ;; updates the new-additions page for the given repository
  (define (build-new-additions-page rep)
    (build-web-page* 
     (NEWEST-PAGE-FILE)
     rep 
     `((p "This is a list of the latest " 
          ,(number->string PKGS-PER-PAGE)
          " packages uploaded to PLaneT "
          ,(format "for the PLT Scheme v~a repository, newest first." 
                   (version->description (repository-name rep)))))
     (repository->newest-first-page rep)))
  
  (define (build-web-page* file rep header-blurb contents)
    (parameterize ((current-write-page 
                    (lambda (x y) 
                      (make-directory* (build-path (WEBROOT) (repository-name rep)))
                      (let ((out (open-output-file
                                  (build-path (WEBROOT) (repository-name rep) file)
                                  'replace)))
                        (write-xml/content (xexpr->xml y) out)
                        (close-output-port out)))))
      (write-tall-page 
       (format "http://planet.plt-scheme.org/~a/" (repository-name rep))
       (format "PLaneT Package Repository")
       (generate-web-page rep 
                          (append
                           header-blurb
                           `((p "You can be notified when new packages are added by subscribing to the "
                              (a ((href ,(url->string (repository->rss-url rep)))) "RSS feed")
                              " or to the " 
                              (a ((href ,(url->string (MAIL-SUBSCRIBE-URL)))) "PLaneT-Announce mailing list") ".")))
                          contents)
       (list (make-hubs-panel #f #f))
       `((link ((rel "alternate")
                (type "application/rss+xml")
                (title "RSS")
                (href ,(url->string (repository->rss-url rep)))))))))
  
  ;; generate-web-page : repository (listof xexpr[html]) (listof web-contents) -> listof xexpr[xhtml]
  ;; makes the body of a web page telling all currently-available packages
  (define (generate-web-page rep header-blurb contents)
    (list*
     `(div ((class "nav") (align "right"))
           (small
           ,@(join
              (map (lambda (r)
                     (if (eq? r rep)
                         (format "[~a]" (version->description (repository-name r)))
                         `(a ((href ,(url->string (repository->url r)))) ,(version->description (repository-name r)))))
                   (get-all-repositories))
              `(nbsp "|" nbsp))
           (br)
           (a ((href ,(WEB-PAGE-FILE))) "Packages by Name")
           nbsp
           "|"
           nbsp
           (a ((href ,(NEWEST-PAGE-FILE))) "Packages by Date Added")))
     `(div ((class "name")) (p ,(format "PLaneT repository for PLT Scheme v~a" 
                                        (version->description (repository-name rep)))))
     `(div ((class "description")) ,@header-blurb)
     (make-tall-page-section "Available Packages")
     (web-contents->table contents)
     (make-tall-page-section "Available Packages: Detail")
     (web-contents->xexprs contents)))
  
  ;; web-contents->table : (listof web-contents) -> xexpr[html table]
  ;; builds a summary table of all the packages available.
  (define (web-contents->table wcl)
    
    ; installed-package -> xexpr[table row]
    (define (package->table-row pkg)
      `(tr ((bgcolor "#ddddff")) 
           (td ((valign "top")) 
               nbsp
               (a ((href ,(format "#~a" (pkg->anchor pkg)))) ,(package-name pkg)))
           (td ((valign "top"))
               ,(format "~a (~a.~a)" 
                        ((installed-package->info pkg) 'version "")
                        (package-maj pkg)
                        (package-min pkg)))
           (td ((valign "top")) ,(package-owner pkg))
           #;(td ((valign "top")) ,(pkg->datestr pkg))
           (td ((valign "top")) ,@(pkg->blurb pkg))))
    
    (define new?-ht (make-hash-table))
    (define (new? p) (hash-table-get new?-ht p (lambda () (hash-table-put! new?-ht p #f) #t)))
    
    ; web-contents -> listof xexpr[table row]
    (define (wc->trs wc)
      (cond
        [(installed-package? wc)
         (if (new? wc)
             (list (package->table-row wc))
             '())]
        [else 
         (let ((the-rest (apply append (srfi1:filter-map wc->trs (cdr wc)))))
           (if (pair? the-rest)
               (cons `(tr (td ((colspan "4")) (b ,(car wc)))) the-rest)
               '()))]))
    
    `(table ((width "100%"))
            (tbody
             ,@(apply append (map wc->trs wcl)))))
  
  ;; web-contents->xexpr : listof web-contents -> listof xexpr
  (define (web-contents->xexprs wcl)
    (define (wc->xexpr wc)
      (let loop ((wc wc)
                 (depth 1))
        (cond
          [(installed-package? wc)
           `(div ((class "package")
                  (style "background-color: #f3f4ff; padding-left: 10px; margin-left: 10px; margin-right: 30px; margin-bottom: 10px;"))
                 (a ((name ,(pkg->anchor wc)))
                    (b ,(package-name wc)) 
                    " contributed by " ,(package-owner wc)
                    ,@(let ((homepageurl ((installed-package->info wc) 'homepage #f)))
                        (if (string? homepageurl)
                            `(nbsp "[" (a ((href ,homepageurl)) "library home page") "]")
                            '())))
                 (div ((class "latestVersion"))
                      ,(if (documented? wc)
                           `(a ((href ,(url->string (pkg->doc-url wc)))) "documentation")
                           `(span ((class "noDocs")) "[no documentation available]"))
                      nbsp "-" nbsp
                      ,@(let ((v ((installed-package->info wc) 'version #f)))
                          (if v
                              `("latest version: " 
                                ,(format "~a" v)
                                nbsp "-" nbsp)
                              '()))
                      "latest package version: " ,(format "(~a.~a)" (package-maj wc) (package-min wc)))
                 (div ((class "toLoad"))
                      (tt ,(format "(require (planet ~s (~s ~s ~s ~s)))" 
                                   ((installed-package->info wc) 'primary-file "[file]")
                                   (package-owner wc)
                                   (package-name wc) 
                                   (package-maj wc)
                                   (package-min wc))))
                 (p)
                 (div ((style "blurb")) ,@(pkg->blurb wc)))]
          [else
           `(div 
             ((class ,(format "category~a" depth)))
             (h2 ,(car wc))
             ,@(map (lambda (x) (loop x (add1 depth))) (cdr wc)))])))
    (map wc->xexpr wcl))
  
  ;; ============================================================
  ;; UTILITY
  ;; Functions and syntax that are just generally helpful.
  
  #| 
comp: syntax to do an effects-only list-comprehension-esque iteration:

(comp ((id LIST-EXPR) ...) EXPR ...)

runs EXPR with id bound to each possible choice of LIST-EXPR. If there
are multiple id/list-expr clauses, later expressions have earlier ids
lexically bound and are run once for each possible choice of ids further
up the chain.

Returns void, so the only use of the innermost expr is for its effects.

EG:  
(comp ((a '(1 2)) (b (map (lambda (x) (cons a x)) '(3 4))))
      (printf "~a\n" b))
=>
void

Printed:

(1 . 3)
(1 . 4)
(2 . 3)
(2 . 4)

|#
  (define-syntax (comp stx)
    (syntax-case stx ()
      [(_ ((id expr) (id2 expr2) ...) body-expr body-expr2 ...)
       (identifier? #'id)
       #'(let ((options expr))
           (for-each
            (lambda (id)
              (_ ((id2 expr2) ...) body-expr body-expr2 ...))
            options))]
      [(_ () body-expr body-expr2 ...)
       #'(begin body-expr body-expr2 ...)]))
  
  
  
  ;; insert : X (listof X) (X X -> Bool) -> (listof X)
  ;; side condition: < is an ordering and l is ordered with respect to <
  ;; inserts i into l such that the result is still ordered
  (define (insert i l <)
    (cond
      [(null? l) (list i)]
      [(< i (car l)) (cons i l)]
      [else (cons (car l) (insert i (cdr l) <))]))
  
  ; extract-max : (nonempty-listof X) (X X -> bool) -> X
  ; gets the item that is largest wrt the given ordering in the given list
  (define (extract-max neloi <)
    (let loop ((best (car neloi)) (xs (cdr neloi)))
      (cond
        [(null? xs) best]
        [else (loop (if (< best (car xs))
                        (car xs)
                        best) 
                    (cdr xs))])))
  
  ; take-up-to : (listof X) * nat -> listof X
  ; takes up to the given number of items from l, returning (a copy of) l
  ; if its length is <= n
  (define (take-up-to l n)
    (cond
      [(null? l) '()]
      [(zero? n) '()]
      [else (cons (car l) (take-up-to (cdr l) (sub1 n)))]))
  
  ;; join : (listof X) (listof Y) -> (listof (union X Y))
  ;; returns a list consisting of the elements of xs in order with the complete sequence of joint between each pair
  (define (join xs joint)
    (cond
      [(null? xs) '()]
      [else
       (let loop ((xs xs))
         (cond
           [(null? (cdr xs)) xs]
           [else (append (list (car xs)) joint (loop (cdr xs)))]))]))
  
  
  )