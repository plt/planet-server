(module repository-types mzscheme
  (require (lib "contract.ss")
           (lib "getinfo.ss" "setup")
           (lib "xml.ss" "xml")
           (lib "url.ss" "net")
           (lib "match.ss")
           (lib "date.ss")
           "server-config.ss")
  
  (provide 
   (struct repository (name directory cache))
   info?
   (struct package (name lang owner maj min))
   (struct installed-package (date-added repository))
   notifier?)
  
  ;; ============================================================
  ;; This module provides structures and predicates that will
  ;; be useful to all parts of the repository-update code.

  ;; a repository is the notion of the packages available to a particular language
  ;; there should generally be one of these available for every language the planet
  ;; server supports
  (define-struct repository 
                 (name      ; string
                  directory ; path[relative,directory]
                  cache     ; repository-cache
                  )
                 (make-inspector))
  
  ; info ::= (symbol (-> TST) -> TST)
  ; info is the type of info functions package caches
  (define info? (symbol? any/c . -> . any/c))

  ;; packages are a representation of a package. It may or may not actually be
  ;; installed anywhere.
  (define-struct package
                 (name     ;; string : name of the package (invariant: also the name of the file it resides in)
                  lang     ;; (listof string) : languages this package supports (currently unused)
                  owner    ;; string : name of the package maintainer
                  maj    ;; nat    : major version
                  min    ;; nat    : minor version
                  )
    (make-inspector))

  ;; installed-packages are packages that are actually in some repository somewhere
  (define-struct (installed-package package)
                 (date-added ; nat?         : current seconds when the package was created (sometimes a guess)
                  repository ; repository?  : the repository in which this package is installed
                  cached-info; info? | #f
                  )
    (make-inspector))
  
  ;; notifier ::= (listof installed-package) -> void
  ;; notifiers are functions that announce a new installation to the world.
  ;; invariant: each element in the list represents the same exact package installed in
  ;; different repositories. 
  (define notifier? ((listof installed-package?) . -> . void?))
  
  ;; ============================================================
  ;; SOME HELPERS
  ;; Some of these don't honestly belong here, but they're needed by
  ;; multiple modules and this is the easiest place to put them
  
  
  (provide/contract (installed-package->readable (installed-package? . -> . any)))
  ; installed-package->readable : installed-package -> any? [readable]
  ; gives a marshalled representation of the given package
  (define (installed-package->readable pkg)
    (map (lambda (a) (a pkg))
         (list
          package-name
          package-lang 
          package-owner
          package-maj 
          package-min 
          installed-package-date-added)))
  
  (provide/contract (readable->installed-package (any/c repository? . -> . installed-package?)))
  ;; readable->installed-package : any?[result of marshall-installed-package] -> installed-package
  (define (readable->installed-package readable rep)
    (match readable
      [((? string? name)
        (? string? lang)
        (? string? owner)
        (? integer? maj)
        (? integer? min)
        (? integer? date-added))
       (make-installed-package name lang owner maj min date-added rep #f)]
      [_ (error 'readable->installed-package "given illegal object: ~s" readable)]))
    
  
  ; installed-package->info : installed-package -> info
  ; gets the info for the specified version of the specified cacheval
  (provide/contract
   (installed-package->info (installed-package? . -> . info?)))
  (define (installed-package->info pkg)
    (unless (installed-package-cached-info pkg)
      (let ((infofn
             (let ((i (get-info/full (pkg->absolute-path pkg))))
               (if i
                   (lambda (x default) (i x (lambda () default)))
                   (lambda (x default) default)))))
        (set-installed-package-cached-info! pkg infofn)))
    (installed-package-cached-info pkg))

  (provide/contract (pkg->blurb (installed-package? . -> . (listof xexpr?))))
  ;; gets an HTML blurb for the given package
  (define (pkg->blurb pkg)
    (let ((blurb ((installed-package->info pkg) 'blurb "[no description available]")))
      (if (string? blurb)
          (list blurb)
          blurb)))
  
  (provide/contract (pkg->anchor (installed-package? . -> . string?)))
  ;; gets an achor for the given package  
  (define (pkg->anchor pkg)
    (format "~a~a.~a" (package-name pkg) (package-maj pkg) (package-min pkg)))
  
  ;; installed-package installed-package -> boolean
  ;; ordering for packages, newest (biggest date) first
  (provide/contract (pkg< (installed-package? installed-package? . -> . boolean?)))
  (define (pkg< a b)
    (> (installed-package-date-added a)
       (installed-package-date-added b)))
  
  ; pkg->relative-path/list : package -> (listof path[relative])
  ; returns a relative path that will be unique to a particular package version within
  ; some root directory, assuming all subdirectories off that root were made using
  ; this function
  (provide/contract (pkg->relative-path/list (package? . -> . (listof path?))))
  (define (pkg->relative-path/list pkg)
    (list (string->path (package-owner pkg))
          (string->path (package-name  pkg))
          (string->path (number->string (package-maj pkg)))
          (string->path (number->string (package-min pkg)))))
  
  ;; pkg->relative-path : package -> path[file,relative]
  ;; gets a relative path for this package
  (provide/contract (pkg->relative-dir-path (package? . -> . path?)))
  (define (pkg->relative-dir-path pkg)
    (apply build-path (pkg->relative-path/list pkg)))
  
  (provide/contract (pkg->absolute-path (installed-package? . -> . path?)))
  (define (pkg->absolute-path pkg)
    (build-path (PLANET-SERVER-REPOSITORY)
                (repository-directory (installed-package-repository pkg))
                (pkg->relative-dir-path pkg)))
  
  ; file-path : installed-package -> path[absolute]
  ; gives the path to a repository file file containing the specified package
  ; Doesn't check to make sure it exists
  (provide/contract (pkg->absolute-file-path (installed-package? . -> . path?)))
  (define (pkg->absolute-file-path pkg)
    (build-path (pkg->absolute-path pkg)
                (package-name pkg)))
  
  (provide/contract (pkg->doc-dir (installed-package? . -> . path?)))
  ;; installed-package -> path[absolute,directory]
  ;; path to the location on disk of this package's doc directory
  (define (pkg->doc-dir p)
    (build-path (WEBROOT) 
                (repository-directory (installed-package-repository p))
                (DOCS-DIR)
                (pkg->relative-dir-path p)))
  
 
  (provide/contract (repository->url (repository? . -> . url?)))
  ;; the url for the web page containing info on the given repository
  (define (repository->url rep)
    (make-url
     "http" #f (PLANET-WEB-HOST) (PORT)
     `(,@(PLANET-WEB-PATH)
         ,(repository-name rep)
         "")
     '()
     #f))
  
  
  (provide/contract (pkg->url (installed-package? . -> . url?)))
  ;; url for this package
  (define (pkg->url pkg)
    (let ((repository-url (repository->url (installed-package-repository pkg))))
      (combine-url/relative repository-url (string-append "#" (pkg->anchor pkg)))))
  
  
  (provide/contract (pkg->doc-url (installed-package? . -> . url?)))
  ;; url for this package's documentation
  (define (pkg->doc-url p)
    (make-url "http" #f (PLANET-WEB-HOST) (PORT)
              `(,@(PLANET-WEB-PATH)
                  ,(repository-name (installed-package-repository p))
                  ,(DOCS-DIR) 
                  ,@(map path->string (pkg->relative-path/list p))
                  "doc.txt")
              '()
              #f))
  
  (provide/contract (pkg->datestr (installed-package? . -> . string?)))
  ;; gives an rfc-822-compliant date string representing when the package
  ;; was installed
  (define (pkg->datestr pkg)
    (parameterize ((date-display-format 'rfc2822))
      (date->string (seconds->date (installed-package-date-added pkg)) #t)))

  (provide/contract (documented? (installed-package? . -> . boolean?)))
  ;; determines if docs were installed
  (define (documented? pkg)
    (file-exists? (build-path (pkg->doc-dir pkg) "doc.txt"))))