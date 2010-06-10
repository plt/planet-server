#lang scheme/base
;current as of 8/25/2008	
(require "db.ss" "data-structures.ss" "configuration.ss"
         "html.ss" "scm2xexpr.scm" "announcements.ss"
	 "cookie-monster.ss")
(require "tracplanet/trac-admin.ss")

(require scheme/contract
         scheme/file
         scheme/path
         scheme/port
         setup/pack
         setup/unpack
         setup/getinfo
         srfi/42
         xml/xml
         scheme/match
         scheme/list)

(define-struct (exn:fail:bad-package exn:fail) (xexprs))

(provide (struct-out exn:fail:bad-package))
(provide/contract
 [planet-file-name? (any/c . -> . boolean?)]
 [create-package
  (user?                    ; package owner
   string?
   bytes?                   ; package file contents
   . -> .
   natural-number/c)]
 [update-package
  (user?                    ; package owner
   package?                 ; package being updated
   boolean?                 ; backwards-compatible update?
   bytes?                   ; package contents
   . -> .
   natural-number/c)]
 [update-non-head-package
  (user?
   package?
   natural-number/c
   bytes?
   . -> .
   natural-number/c)]
 [rebuild-package-pages
  (string? string? natural-number/c natural-number/c
           . -> .
           void?)]
 [rebuild-all-code-pages
  (-> any)]
 [rebuild-all-autoinstallers
  (-> any)])

;; planet-file-name? : any -> boolean
;; determines if the given value represents a planet file name
(define (planet-file-name? s)
  (and (string? s) (regexp-match #rx"^[^.].*\\.plt$" s)))

;; raise-bad-package-error : string -> [raises exn:fail:bad-package]
;; report on a package problem
(define (raise-bad-package-error xexprs)
  (raise (make-exn:fail:bad-package
          (string->immutable-string (format "~s" xexprs))
          (current-continuation-marks)
          xexprs)))

(define (create-package user package-name file-bytes)
  
  (unless (planet-file-name? package-name)
    (raise-bad-package-error 
     `("The file name "(b ,package-name) " is not a valid name for a PLaneT package. PLaneT packages "
                       " must be .plt files created by the " (tt "planet") " command-line tool whose names consist of "
                       "letters, numbers, and hyphens.")))
  
  (when (get-package (user-username user) package-name)
    (raise-bad-package-error
     `("You already have a package named "(b ,package-name)" on PLaneT. If you would like to update that "
                                          "package, please click the \"update this package\" link next to its name.")))
  
  
  (let* ([cats (get-category-names)]
         [cats-ht (make-hasheq)]
         [trac (add-component package-name (user-username user))])
    (for-each 
     (λ (c) (hash-set! cats-ht (category-shortname c) c))
     cats)
    (update/internal user
                     package-name
                     1
                     0
                     file-bytes
                     (λ (i)
                       (let* ([blurb-datum (i 'blurb (λ () #f))]
                              [blurb (if (string? blurb-datum)
                                         (list blurb-datum)
                                         blurb-datum)]
                              [pkg-stub (add-package-to-db! user package-name blurb (i 'homepage (λ () #f)))]
                              [count 0])
                         (for-each
                          (lambda (shortname) 
                            (let ([c (hash-ref cats-ht shortname (λ () #f))])
                              (if c
                                  (begin 
                                    (associate-package-with-category pkg-stub c)
                                    (set! count (add1 count)))
                                  (void) #;(report-bad-category shortname))))
                          (i 'categories (λ () '())))
                         (when (= count 0)
                           (associate-package-with-category pkg-stub (hash-ref cats-ht 'misc)))
                         pkg-stub)))))

(define (update-package user pkg minor-update? file-bytes)
  (let* ([maj+min (get-next-version-number pkg minor-update?)]
         [maj (car maj+min)]
         [min (cdr maj+min)])
    (update/internal user (package-name pkg) maj min file-bytes (λ (_) pkg))))

(define (update-non-head-package user pkg maj file-bytes)
  (let ([min (get-next-version-for-maj pkg maj)])
    (update/internal user (package-name pkg) maj min file-bytes (λ (_) pkg))))

;; get-metainfo : path[directory] -> (symbol (-> TST) -> TST)
;; gets an info.ss -retrieving thunk for the given package which is unpacked in the given directory
(define (get-metainfo unpacked-package-path)
  (let ([default-metainfo (lambda (s t) (t))])
    (with-handlers ([exn:fail? (λ (e) default-metainfo)])
      (let ([metainfo (get-info/full unpacked-package-path)])
        (or metainfo
            default-metainfo)))))

;; move-directory-to-tmp : path format-string (one slot) -> path
;; moves the given directory to an out-of-the-way spot and returns where it went
(define (move-directory-to-tmp dir-to-move template)
  (let loop ([i 0])
    (let ([dir (build-path (find-system-path 'temp-dir) (format template i))])
      (cond
        [(directory-exists? dir)
         (loop (add1 i))]
        [else
         (rename-file-or-directory dir-to-move dir)
         dir]))))

(define (status str . fmt)
   (printf "package-creation.ss: ~a\n" (apply format str fmt))
   (flush-output) 
   (flush-output (current-error-port)))

;; update/internal : user? string nat nat bytes ((listof xexpr) -> pkg) -> void
(define (update/internal user pkgname maj min file-bytes getpkg)
  (let* ([username (user-username user)]
         
         ;; unpack in a temporary spot -- this makes sure the package is valid before
         ;; we write into permanent storage
         [tmpdir (make-temporary-file "planettmpdir~a" 'directory)]
         [tmpfilepath (build-path tmpdir pkgname)]
         [tmpsrcdir (build-path tmpdir "contents")])
    (dynamic-wind
     void
     (λ ()
       (let* ([_ (status "starting to unpack")]
              [_ (with-output-to-file tmpfilepath (lambda () (write-bytes file-bytes)) #:exists 'truncate/replace)]
              [_ 
               (with-handlers ([exn:fail?
                                (λ (e) 
                                  ((error-display-handler) 
                                   (format "When unpacking a package, intercepted this exception:\n~a" 
                                           (exn-message e))
                                   e)
                                  (raise-bad-package-error
                                   `("The given package appears to be malformed. Please ensure that the file "
                                     "you are uploading is a .plt file that was built with the "(tt "planet")
                                     " command-line tool.")))])
                 (unpack-planet-package tmpfilepath tmpsrcdir))]
              [_ (status "finished unpacking")]
              
              ;; once we've unpacked, we're sure that the package is valid (at least valid enough)
              ;; so create permanent directories and move things over to them.
              [pkgdir (create-package-directory username pkgname maj min)]
	      [_ (status "created package directory")]
              [permanent-file-path (build-path pkgdir pkgname)] 
              [srcdir (build-path pkgdir "contents")]
              
              [_ (when (file-exists? permanent-file-path)
                   (let ([relocated-file-location (make-temporary-file (string-append username "-" 
                                                                                      pkgname "-" 
                                                                                      (number->string maj) "-" 
                                                                                      (number->string min) "-~a.plt")
                                                                       permanent-file-path)])
                     (delete-file permanent-file-path)
                     (printf 
                      (string-append "~a: When trying to unpack a package, discovered that "
                                     "another file of the same name already exists. Moving "
                                     "that file out of the way (to ~a) under the assumption "
                                     "that it is the result of an earlier crashed run.\n")
                      (current-seconds)
                      (path->string relocated-file-location))))]
              [_ (when (directory-exists? srcdir)
                   (let ([relocated-path-location 
                          (move-directory-to-tmp srcdir (format "~a-~a-~a-~a-~~a-contents" username pkgname maj min))])
                     (printf 
                      (string-append "~a: When trying to unpack a package, discovered that "
                                     "the package's contents directory already exists. Moving "
                                     "that directory out of the way (to ~a) under the assumption "
                                     "that it is the result of an earlier crashed run.\n")
                      (current-seconds)
                      (path->string relocated-path-location))))]
              [_ (copy-file tmpfilepath permanent-file-path)]
              [_ (rename-file-or-directory tmpsrcdir srcdir)]
              [_ (status "getting metadata")]
              ;; get metainfo, add to database
              [info.ss (get-metainfo srcdir)]
              [_ (status "got info.ss")]
              [pkg (getpkg info.ss)]
              [repository-strings (info.ss 'repositories (λ () #f))]
              [repositories
               (let ([all-repositories (get-all-repositories)])
                 (if repository-strings 
                     (filter (λ (x) (member (repository-name x) repository-strings)) all-repositories)
                     all-repositories))]
              [id (add-pkgversion-to-db! user 
                                         pkg
                                         maj
                                         min
                                         permanent-file-path
                                         srcdir
                                         info.ss)])
	 (status "added to database")
         (for-each (λ (r) (associate-pkgversion-with-repository! id r)) repositories)
         (for-each (λ (task) (status "post-install ~s" task)
                             (task srcdir username pkgname maj min)
                             (status "post-install ~s done" task))
                    *post-install-tasks*)
         (when (ANNOUNCE-NEW-PACKAGES?)
           (let ([pkgversion (get-package-version-by-id id (user-id user))])
             (unless pkgversion
               (error 'update/internal "no pkgversion, even though we just added it!"))
             (announce-new-pkgversion pkg pkgversion)))
         id))
     (λ () (delete-directory* tmpdir)) ;; regardless of how we exit, clean up the tmp dir
     )))

(define (setup-source-code-html srcdir username pkgname maj min)
  (let ([webdir (create-web-directory username pkgname maj min)])
    (code-to-html srcdir webdir username pkgname maj min)))

(define (setup-autoinstaller srcdir username pkgname maj min)
  (let ([info.ss (get-metainfo srcdir)]
        [autoinstall-dir (create-autoinstaller-directory username pkgname maj min)]
        [prefix (regexp-match #rx"^(.*)\\.plt$" pkgname)])
    (unless prefix (error "package name did not have expected shape"))
    (let* ([pkgname-prefix (cadr prefix)]
           [autoinstaller-name (format "~a-~a-~a-~a.plt" username pkgname-prefix maj min)]
           [autoinstaller-path (build-path autoinstall-dir autoinstaller-name)]
           
           [package-description (format "~a" (info.ss 'name (λ () pkgname)))])
      
      (build-autoinstaller autoinstaller-path package-description username pkgname maj min))))                  

(define *post-install-tasks* (list setup-source-code-html setup-autoinstaller))

;; rebuild-all-code-pages : -> void
;; rebuilds the entire code pages site.
(define (rebuild-all-code-pages)
  (for-each-package-version
   (λ (pkg pv)
     (let ([user (package-owner pkg)]
           [name (package-name pkg)]
           [maj (pkgversion-maj pv)]
           [min (pkgversion-min pv)])
       (let ([webdir (create-web-directory user name maj min)])
         (printf "deleting ~s\n" webdir)
         (delete-directory* webdir)
         (printf "building ~a/~a/~a/~a\n" user name maj min)
         (rebuild-package-pages user name maj min))))))

;; rebuild-package-pages : string string nat nat -> void
;; rebuilds the web pages for the given package
;; assumes the package is already unpacked in its appropriate location
(define (rebuild-package-pages user package maj min)
  (let ([dir (build-path (create-package-directory user package maj min) "contents")])
    (when (directory-exists? dir) ;; this test should only fail if the db has a bogus entry
      (code-to-html 
       dir
       (create-web-directory user package maj min)
       user package maj min))))

;; rebuild-all-autoinstallers : -> void
;; creates the appropriate autoinstaller for every package version
(define (rebuild-all-autoinstallers)
  (for-each-package-version
   (λ (pkg pv)
     (let ([user (package-owner pkg)]
           [name (package-name pkg)]
           [maj (pkgversion-maj pv)]
           [min (pkgversion-min pv)])
       (let ([autoinstaller-dir (create-autoinstaller-directory user name maj min)])
         (printf "deleting ~s\n" autoinstaller-dir)
         (delete-directory* autoinstaller-dir)
         (printf "building autoinstaller for ~a/~a ~a.~a\n" user name maj min)
         (setup-autoinstaller (pkgversion-src-path pv)
                              user name maj min))))))


(define (make-creator root-dir-param)
  (lambda (usr pkgname maj min)
    (let ([dir (build-path (root-dir-param) usr pkgname (number->string maj) (number->string min))])
      (make-directory* dir)
      dir)))
(define create-package-directory (make-creator FILE-STORAGE-PATH))
(define create-web-directory (make-creator WEB-PACKAGES-ROOT))
(define create-autoinstaller-directory (make-creator AUTOINSTALLERS-ROOT))

;; unpack-planet-package : path? path? -> void
;; unpacks the given package (specified by its path) into the given directory
;; WARNING: I've got to figure out some way to ensure that the .PLT file I'm getting is good, or else
;; this could be used to do arbitrary bad things to the system. (Alternately security guards might 
;; save me here.)
(define (unpack-planet-package plt-file target)
  (parameterize ([current-directory target])
    (unpack plt-file)))

;; ============================================================
;; package web-page generation stuff
;; the scaffold is  stolen from Jens Axel Søgaard,
;; and the actual fontifying engine is from Dorai Sitaram's
;; slatex package by way of Anton van Straaten

(define (write-planet-page file path-to-display usr pkgfile majstr minstr body)
  (let ([body
         (mkhtmlpage (list (list "Home" home-link/base)
                           (list usr (owner-link/fields usr))
                           (list pkgfile (package-link/fields usr pkgfile)) 
                           (list (string-append  " package version " majstr "." minstr)
                                 (source-code-url/fields usr pkgfile majstr minstr)))
                     `(,@(if path-to-display
                             `((section ,(path->string path-to-display)))
                             `())
                       ,body))])
    (with-output-to-file file
      (lambda ()
	(printf "~a\n" doctype)
        (write-xml/content (xexpr->xml body)))
      #:exists 'replace)))

;; Reads all characters from the port until eof and returns the
;; accumulated string [this function was originally from Schematics'
;; port.plt, though I've changed its implementation]
(define (port->string port)
  (let ((output (open-output-string)))
    (copy-port port output)
    (get-output-string output)))

; Handlers for conversion of different formats.
; handle-scm and handle-txt read from current-input-port
; and return an xexpr.
(define (handle-scm)
  (scheme-text->xexpr
   (port->string
    (current-input-port))))

(define (handle-txt)
  (let* ([s           (port->string (current-input-port))]
         [p           (open-input-string s)]
         [first-line  (read-line p)]
         [second-line (read-line p)])
    ; use first or second line as header?
    (let ([header
           (cond
             ; if the first 4 (and likely all) characters
             ; are the same, use the second line in stead
             [(and (string? first-line)
                   (string? second-line)
                   (> (string-length first-line) 4)
                   (every?-ec (: c (substring first-line 0 4))
                              (char=? c (string-ref first-line 0))))
              second-line]
             [(string? first-line)
              first-line]
             [else
              ""])])
      ; remove underscores from header
      (let ([header (list->string
                     (for/list ([c (in-string header)]
                                #:when (not (char=? c #\_)))
                               c))])
        `(div
          (h1 ,header)
          (pre , s))))))

;; ============================================================
;; Copy-and-convert a directory

(define-struct file-info (path modify-seconds size) #:transparent)
(define-struct dir-info  (path 
                          modify-seconds
                          [subdirs #:mutable]
                          [files #:mutable])
  #:transparent)

;; the "/" business here is to hack around a problem
;; with apache configuration that i haven't figured out how to fix yet
;; the problem is that coach.cs is configured at the firewall level to redirect
;; traffic from port 80 to port 8080; this makes apache confused about how
;; to tell the world about where it lives. That's not a problem unless it
;; generates a redirect, at which point it erroneously puts a bad port number
;; in, so to fix that we make sure that it doesn't have to generate redirects.
;; the most common redirect is from http://www/foo to http://www/foo/, so we
;; must be sure to put / marks at the end of each path. This is good practice
;; anyway.
(define (path->linker suffix)
  (λ (path text)
    `(a ((href ,(string-append (path->string path) suffix))) ,text)))

(define path->link (path->linker ""))
(define path->link/ (path->linker "/"))


(define (path<? path1 path2)
  (string<? (path->string path1)
            (path->string path2)))

(define (dir-info<? di1 di2)
  (path<? (dir-info-path di1)
          (dir-info-path di2)))

(define (file-info<? fi1 fi2)
  (path<? (file-info-path fi1)
          (file-info-path fi2)))


(define (remove-trailing-backslashes s)
  (let ([len (string-length s)])
    (if (eqv? (string-ref s (- len 1)) #\\)
        (remove-trailing-backslashes 
         (substring s 0 (- len 1)))
        s)))

(define (parent-path dir-path)
  (if (equal? dir-path "")
      (build-path ".")
      (let-values ([(base name _) (split-path dir-path)])
        (if (eq? base 'relative) 
            (build-path ".")
            (let ([s (path->string base)])
              (string->path (remove-trailing-backslashes s)))))))

(define (prepare path)
  (regexp-replace #rx"/$" (path->bytes path) #""))

(define (path->listof-bytes p)
  (let ([splits (regexp-split #rx"/" (path->bytes p))])
    (cond
      [(bytes=? (car splits) #"")
       (cons #"/" (cdr splits))]
      [else splits])))

(define (trim-prefix a b)
  (cond
    [(or (null? a) (null? b)) b]
    [(equal? (car a) (car b))
     (trim-prefix (cdr a) (cdr b))]
    [else b]))

(define (collect-info-on-source-files root-path)
  (define info (make-hash))
  (define (robust-file-size path)
    (with-handlers
        ([(lambda (o) #t) (lambda (o) 0)])
      (file-size path)))
  
  (define root-path-list (path->listof-bytes root-path))
  (define (trim-to-extension path)
    (bytes->path (car (last-pair (regexp-split #rx"/" (path->bytes path))))))
  
  ; enter all paths in hash-table
  (parameterize ([current-directory root-path])
    (hash-set! info #"." (make-dir-info "" 0 '() '()))
    (fold-files 
     (lambda (path type acc)
       (hash-set! info (prepare path) 
                  (case type
                    [(dir) (make-dir-info (trim-to-extension path)
                                          (file-or-directory-modify-seconds path)
                                          '() '())]
                    [else  'skip])))
     'init-val
     #f)
    
    ; add info on files
    (fold-files
     (lambda (path type acc)
       (let ([source-path (build-path root-path path)])
         (case type
           [(file)
            (let ([fi (make-file-info
                       (trim-to-extension path)
                       (file-or-directory-modify-seconds path)
                       (robust-file-size path))])
              (hash-set! info (prepare path) fi)
              (let ([di (hash-ref info 
                                  (prepare (parent-path path))
                                  (print-info info (path->bytes (parent-path path)) 1))])
                (set-dir-info-files! di (cons fi (dir-info-files di)))))]
           [(dir)   
            (let ([di (hash-ref info (prepare (parent-path path)) (λ () #f))])
              (when di
                (set-dir-info-subdirs! 
                 di 
                 (cons (hash-ref info (prepare path) (print-info info (prepare path) 2))
                       (dir-info-subdirs di)))))]
           [(link)   
            (error "No links should occur in the source directory")]
           [else 
            (error "Unknown type")])))
     'init-val
     #f)) ; #f => path relative to current-directory
  info)

(define (print-info info path i)
  (lambda ()
    (printf "location ~a\n" i)
    (hash-for-each
     info
     (λ (k v) (printf "~s => ~s\n" k v)))
    (error 'html
           (string->immutable-string
            (format
             "Couldn't find the path ~s in info table [it should always be there]" path)))))

(define (code-to-html src-directory target-directory usr pkgname maj min)
  ;; copy source files to target directory, convert them to html as appropriate
  (let ([majstr (number->string maj)]
        [minstr (number->string min)]
        [info (collect-info-on-source-files src-directory)])
    (parameterize ([current-directory src-directory])
      (fold-files 
       (lambda (path type acc)
         ; path is relative
         (let ([source-path (build-path src-directory path)]
               [html-path   (build-path target-directory path)])
           (case type
             [(file)
              (let ([ext (filename-extension source-path)]
                    [too-big? (>= (file-size source-path) (* 100 1024))])
                (when too-big?
                  (status "code-to-html skipping ~s" source-path))
                (if (or (not (member ext '(#"ss" #"scm" #"txt")))
                        too-big?)
                    ; copy file
                    (begin
                      (when (file-exists? html-path)
                        (delete-file html-path))
                      (copy-file source-path html-path))
                    ; convert to html
                    (with-input-from-file source-path
                      (lambda ()
                        (let ([xexpr (cond
                                       [(member ext '(#"ss" #"scm"))   (handle-scm)]
                                       [(equal? ext #"txt")            (handle-txt)]
                                       [else                           (error)])])
                          (write-planet-page html-path path
                                             usr pkgname majstr minstr
                                             xexpr))))))]
             [(dir)  
              (begin
                (make-directory* html-path)
                (write-planet-page (build-path html-path "planet-browser-index.html") path
                                   usr pkgname majstr minstr
                                   (handle-package-index info path)))])))
       'init-val
       #f)
      ;; build the package's home index
      (write-planet-page (build-path target-directory "planet-browser-index.html") #f
                         usr pkgname majstr minstr
                         (handle-package-index info (string->path "."))))))

(define (handle-package-index info path)
  (handle-index (hash-ref info (prepare path) (print-info info (prepare path) 3))))

(define (handle-index di)
  `(div
    ,(make-directory-list "Directories" di)
    ,(make-file-list      "Files"       di)))

(define (make-directory-list header di #:path->link-text [path->link-text path->string])
  (match di
    [(struct dir-info (path modify-seconds subdirs files))
     `(div
       (h2 ,header)
       (ul 
        #;,@(list (if skip-dotdot (list)
                      `(li ,`(a ((href ,(string-append (*BASE-URL*) (parent-path->string path))))
                                ".."))))
        ,@(list-ec (: subdir (sort subdirs dir-info<?))
                   (:let path (dir-info-path subdir))
                   `(li ,(path->link/ (dir-info-path subdir) 
                                      (path->link-text (dir-info-path subdir)))))))]
    [else
     (error di)]))

(define (make-file-list header di)
  (match di
    [(struct dir-info (path modify-seconds subdirs files))
     `(div
       ,@(if (null? files) '()
             (list 
              '(h2 "Files")
              `(ul
                ,@(list-ec (: file (sort files file-info<?))
                           `(li ,(path->link (file-info-path file) 
                                             (path->string (file-info-path file)))))))))]
    [else
     (error di)]))

;; ============================================================
;; Autoinstaller stuff




;; build-autoinstaller : path[full path to plt file]
;;                       string[package description]
;;                       string[owner]
;;                       string[package]
;;                     -> void
;; SIDE EFFECT: writes an autoinstaller to path.
(define (build-autoinstaller plt-filename package-name owner pkgname maj min)
  (define spec `(require "dummy-file.ss" (,owner ,pkgname ,maj ,min)))
  (pack-plt plt-filename
            (format "~a (PLaneT package version ~a.~a auto-installation bundle)" package-name maj min)
            '()
            #:unpack-unit
            `(unit (import collects-dir unmztar)
                   (export)
                   (begin
                     (let ([get (dynamic-require `(lib "resolver.ss" "planet") 'get-planet-module-path/pkg)])
                       (get ',spec #f #f))
                     (void))
                   (list))))

;; ============================================================
;; misc utility

;; delete-directory* : path -> void
;; rm -rf a directory.
(define (delete-directory* path)
  (cond
    [(link-exists? path)
     (error 'delete-directory* "found a symbolic link, which isn't supposed to exist here: ~e" path)]
    [(file-exists? path)
     (delete-file path)]
    [(directory-exists? path)
     (for-each
      (λ (e) (delete-directory* (build-path path e)) (directory-list path))
      (directory-list path))
     (delete-directory path)]
    [else 
     (error 'delete-directory* "Given path does not exist: ~e" path)]))
