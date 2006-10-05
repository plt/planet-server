(module package-adder mzscheme
  ;; this module's job is to unpack, set up, etc planet packages upon submission.
  (require (lib "contract.ss"))
  (require (lib "file.ss"))
  (require (lib "unpack.ss" "setup"))
  
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
           [permanent-file-path (build-path pkgdir file)] ;; nb we must be sure that the file name is safe, or this will be an exploit!!!!
           [srcdir (build-path pkgdir "contents")]
           [webdir (create-web-directory user-pkgname maj min)])
      ;;   2. Drop the file into its new home
      (copy-file file permanent-file-path)
      ;;   2. Unpack the package into the package directory
      (unpack-planet-package file srcdir)
      ;;   3. Run the fontifier on the contents of the directory and move it to a public html location
      (code-to-html srcdir webdir user pkgname maj min)
      permanent-file-path))
  
  (define *FILE-STORAGE-PATH* (make-parameter (build-path "/Users/jacobm/tmp/planet-files")))
  (define *WEB-PACKAGES-ROOT* (make-parameter (build-path "/Users/jacobm/svn/plt/collects/web-server/default-web-root/htdocs/planet-packages/")))
  
  (define (make-creator root-dir-param)
    (lambda (usr pkgname maj min)
      (let ([dir (build-path (root-dir-param) usr pkgname (number->string maj) (number->string min))])
        (make-directory* dir)
        dir)))
  (define create-package-directory (make-creator *FILE-STORAGE-PATH*))
  (define create-web-directory (make-creator *WEB-PACKAGES-ROOT*))
  
  ;; unpack-planet-package : path? path? -> void
  ;; unpacks the given package (specified by its path) into the given directory
  ;; WARNING: I've got to figure out some way to ensure that the .PLT file I'm getting is good, or else
  ;; this could be used to do arbitrary bad things to the system. (Alternately security guards might 
  ;; save me here.)
  (define (unpack-planet-package plt-file target)
    (parameterize ([current-directory target])
      (unpack plt-file)))
  
  (define (code-to-html src-directory target-directory usr pkgname maj min)
    (void))
    
    )
