(module package-adder mzscheme
  ;; this module's job is to unpack, set up, etc planet packages upon submission.
  (require (lib "contract.ss")
           (lib "file.ss")
           (lib "port.ss")
           (lib "unpack.ss" "setup")
           (lib "42.ss" "srfi")
           (lib "xml.ss" "xml")
           (lib "kw.ss")
           (lib "match.ss")
           (lib "list.ss")
           
           "data-structures.ss"
           "html.ss"
           "configuration.ss"
           "scm2xexpr.scm")
  
  (provide/contract
   ;; store-package : stores the given file as a package.
   [store-package (path?   #| path to a temporary file  |#
                   user?   #| owner |# 
                   string? #| package name |# 
                   natural-number/c #| maj |#
                   natural-number/c #| min |#
                   . -> .
                   path? #| where it was installed; i'm not actually really sure what to return here |#)])
  
  (define (store-package file user pkgname maj min)
    ;; tasks:
    ;;   1. Create a new web directory and home directory (non web-accessible) for the package
    (let* ([pkgdir (create-package-directory user pkgname maj min)]
           [permanent-file-path (build-path pkgdir pkgname)] ;; nb we must be sure that the file name is safe, or this will be an exploit!!!!
           [srcdir (build-path pkgdir "contents")]
           [webdir (create-web-directory user pkgname maj min)])
      ;;   2. Drop the file into its new home
      (copy-file file permanent-file-path)
      ;;   2. Unpack the package into the package directory
      (unpack-planet-package file srcdir)
      ;;   3. Run the fontifier on the contents of the directory and move it to a public html location
      (code-to-html srcdir webdir user pkgname (number->string maj) (number->string min))
      permanent-file-path))
  
  (define (make-creator root-dir-param)
    (lambda (usr pkgname maj min)
      (let ([dir (build-path (root-dir-param) (user-username usr) pkgname (number->string maj) (number->string min))])
        (make-directory* dir)
        dir)))
  (define create-package-directory (make-creator FILE-STORAGE-PATH))
  (define create-web-directory (make-creator WEB-PACKAGES-ROOT))
  
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
           (mkhtmlpage (list (user-username usr) pkgfile majstr minstr)
                       `(,@(if path-to-display
                               `((section ,(path->string path-to-display)))
                               `())
                         ,body))])
      (with-output-to-file file
        (lambda ()
          (write-xml/content (xexpr->xml body))))))
  
   
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
        (let ([header (string-ec (: c header)
                                 (if (not (char=? c #\_)))
                                 c)])
          `(div
            (h1 ,header)
            (pre , s))))))
  
  ;; ============================================================
  ;; Copy-and-convert a directory
  
  (define-struct file-info (path modify-seconds size) (make-inspector))
  (define-struct dir-info  (path modify-seconds subdirs files) (make-inspector))
  
  (define (path->link path text)
    `(a ((href ,(string-append #;(*BASE-URL*) (path->string path))))
        ,text))
  
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
        ""
        (let-values ([(base name _) (split-path dir-path)])
          (if (eq? base 'relative) 
              ""
              (let ([s (path->string base)])
                (string->path (remove-trailing-backslashes s)))))))

  (define (collect-info-on-source-files root-path)
    (define info (make-hash-table 'equal))
    (define (robust-file-size path)
      (with-handlers
          ([(lambda (o) #t) (lambda (o) 0)])
        (file-size path)))
    
    ; enter all paths in hash-table
    (parameterize ([current-directory root-path])
      (hash-table-put! info "" (make-dir-info "" 0 '() '()))
      (fold-files 
       (lambda (path type acc)
         (hash-table-put! info path 
                          (case type
                            [(dir) (make-dir-info path 
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
             [(file) (let ([fi (make-file-info
                                path
                                (file-or-directory-modify-seconds path)
                                (robust-file-size path))])
                       (hash-table-put! info path fi)
                       (let ([di (hash-table-get info (parent-path path))])
                       (set-dir-info-files! di (cons fi (dir-info-files di)))))]
           [(dir)   (let ([di (hash-table-get info (parent-path path) (λ () #f))])
                      (when di
                        (set-dir-info-subdirs! di 
                                               (cons  (hash-table-get info path)
                                                      (dir-info-subdirs di)))))]
           [(link)   (error "No links should occur in the source directory")]
           [else     (error "Unknown type")]))
       (display (list path type)) (newline))
     'init-val
     #f)) ; #f => path relative to current-directory
    info)
   
  (define (code-to-html src-directory target-directory usr pkgname majstr minstr)
    ;; copy source files to target directory, convert them to html as appropriate
    (let ([info (collect-info-on-source-files src-directory)])
    
      (parameterize ([current-directory src-directory])
        (fold-files 
         (lambda (path type acc)
           ; path is relative
           (let ([source-path (build-path src-directory path)]
                 [html-path   (build-path target-directory path)])
             (case type
               [(file)
                (let ([ext (filename-extension source-path)])
                  (if (not (member ext '(#"ss" #"scm" #"txt")))
                      ; copy file
                      (copy-file source-path html-path)
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
                           (handle-package-index info "")))))
    
  (define (handle-package-index info path)
    (handle-index (hash-table-get info path)))
  
  (define (handle-index di)
  `(div
    ,(make-directory-list "Directories" di)
    ,(make-file-list      "Files"       di)))
  
  (define/kw (make-directory-list header di #:key
                                  (path->link-text path->string)
                                  (skip-dotdot #f)
                                  (skip-collects #f))
    (match di
      [($ dir-info path modify-seconds subdirs files)
       `(div
         (h2 ,header)
         (ul 
          #;,@(list (if skip-dotdot (list)
                      `(li ,`(a ((href ,(string-append (*BASE-URL*) (parent-path->string path))))
                                ".."))))
          ,@(list-ec (: subdir (mergesort subdirs dir-info<?))
                     (:let path (dir-info-path subdir))
                     `(li ,(path->link (dir-info-path subdir) 
                                       (path->link-text (dir-info-path subdir)))))))]
      [else
       (error di)]))
  
  (define (make-file-list header di)
    (match di
      [($ dir-info path modify-seconds subdirs files)
       `(div
         ,@(if (null? files) '()
               (list 
                '(h2 "Files")
                `(ul
                  ,@(list-ec (: file (mergesort files file-info<?))
                             `(li ,(path->link (file-info-path file) 
                                               (path->string (file-info-path file)))))))))]
      [else
       (error di)]))
  
  
  
  )
    