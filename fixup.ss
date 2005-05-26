(module fixup mzscheme
  (require 
   (lib "file.ss")
   (lib "list.ss"))
  
  (provide (all-defined))
  
  (define (subdirs d)
    (filter directory-exists? (map (lambda (x) (build-path d x)) (directory-list d))))
  
  (define (fixup r)
    (for-each fix-person (subdirs r)))
  
  (define (fix-person p)
    (for-each
     (lambda (pkg)
       (when (file-exists? (build-path pkg "info.ss"))
         (for-each
          (lambda (vmaj) 
            (for-each
             (lambda (vmin)
               #;(copy-file (build-path pkg "info.ss") vmin)
               (printf "~s --> ~s\n" (build-path pkg "info.ss") vmin))
             (subdirs vmaj)))
          (subdirs pkg))))
     (subdirs p))))