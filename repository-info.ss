#|
This library provides an interface for code clients to get metainformation
about the repository or planet packages.
|#
(module repository-info mzscheme

  (require "planet-getinfo.ss"
           "server-config.ss"
           (lib "planet-shared.ss" "planet" "private")
           (lib "file.ss")
           (lib "list.ss"))

  (provide (all-defined))
  
  (define-struct owner (name packages))
  (define-struct package (owner-name name maj min blurb doc.txt primary-file))

  ;; metainfo : path -> (symbol X -> X)
  ;; given a path, produces the metainfo function for the given file
  (define (metainfo path)
    (with-handlers ([exn:fail? (lambda (e) (lambda (x y) (y)))])
      (let ((getter (get-info-from-file path)))
        (if getter
            (lambda (item default) 
              (with-handlers ([exn:fail? (lambda (x) (default))])
                (getter item)))
            (lambda (x default) (default))))))

  
  ;; current-repository-contents : -> tree
  ;; produces a tree corresponding to the contents of the current repository
  ;; (all packages for all versions in the repository)
  (define (current-repository-contents)
    (define id (lambda (x) x))
    (apply list
           (cdr
            (tree->list
             (filter-tree-by-pattern
              (directory->tree (PLANET-SERVER-REPOSITORY) 
                               (lambda (x) 
                                 (let ((filename (file-name-from-path x)))
                                   (not (memv filename (list "CVS" (METAINFO-FILE)))))))
              (list id id id id string->number string->number))))))
  
  (define (archive-as-tree version webroot)

    ;; sexp -> owner structure
    (define (owner-line->owner owner)
      (make-owner
       (owner->name owner)
       (quicksort
        (map 
         (lambda (x) (package-line->package x (owner->name owner)))
         (owner->packages owner))
        (lambda (a b) (string<? (package-name a) (package-name b))))))

    ;; sexp -> package structure (as defined at the top of the file)
    (define (package-line->package pkg owner-name)
      
      (define metainfo-file-path (build-path (PLANET-SERVER-REPOSITORY) 
                                             (language-version->repository version)
                                             owner-name
                                             (pkg->name pkg)
                                             (METAINFO-FILE)))
      
      (define metainfo
        (with-handlers ([exn:fail? (lambda (e) (lambda (x y) (y)))])
          (let ((getter (get-info-from-file metainfo-file-path)))
            (if getter
                (lambda (item default) 
                  (with-handlers ([exn:fail? (lambda (x) (default))])
                    (getter item)))
                (lambda (x default) (default))))))
      
      (define description 
        (let ((orig-blurb (metainfo 'blurb (lambda () "No description available."))))
          (if (string? orig-blurb)
              (list orig-blurb)
              orig-blurb)))
      
      (define file-to-require (metainfo 'primary-file (lambda () "[file]")))
      
      (define latest-major-version (apply max (pkg->major-versions pkg)))
      (define latest-minor-version (apply max (pkg->minor-versions pkg latest-major-version)))
      
      (make-package owner-name (pkg->name pkg) latest-major-version latest-minor-version
                    description
                    (if (file-exists? (build-path webroot
                                                  (DOCS-DIR)
                                                  owner-name
                                                  (pkg->name pkg) 
                                                  (number->string latest-major-version)
                                                  (number->string latest-minor-version)
                                                  "doc.txt"))
                        (format "~a/~a/~a/~a/~a/doc.txt"
                                (DOCS-DIR)
                                owner-name
                                (pkg->name pkg)
                                latest-major-version
                                latest-minor-version)
                        #f)
                    file-to-require))
    
    
    (let* ([owners/all-versions (current-repository-contents)]
           [owners (let ((x (assoc version owners/all-versions)))
                     (if x (cdr x) '()))])
      (quicksort
       (map 
        owner-line->owner
        owners)
       (lambda (a b) (string<? (owner-name a) (owner-name b))))))
    
    
  ;; ============================================================
  ;; UTILITIES
  ;; ============================================================
  
  ;; ==============================
  ;; OWNER MANIPULATION
  ;; ==============================
  (define owner->name car)
  (define owner->packages cdr)
  
  ;; ==============================
  ;; PKG MANIPULATION
  ;; ==============================
  (define pkg->name car)
  
  (define (pkg->major-versions pkg)
    (map car (cdr pkg)))
  
  (define (pkg->minor-versions pkg maj)
    (map car (cdr (assq maj (cdr pkg)))))        
  
  
  ;; join : (listof string) string -> string
  ;; joins the given strings with the given separator between each
  (define (join l sep)
    (define the-list
      (let loop ((l l))
        (cond
          [(null? l) '()]
          [(null? (cdr l)) (list (car l))]
          [else (list* (car l) sep (loop (cdr l)))])))
    (apply string-append the-list))
  
  ;; read-file-to-string : path -> string
  ;; reads the contents of the given file into a string. 
  ;; (That's a bad idea for large files, but for small ones it's fine.)
  (define (read-file-to-string path)
    (with-input-from-file path
      (lambda ()
        (let loop ((acc '()))
          (let ((line (read-line)))
            (cond
              [(eof-object? line)
               (apply string-append (reverse acc))]
              [else
               (loop (cons line acc))]))))))


    

  
  )