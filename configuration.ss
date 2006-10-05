(module configuration mzscheme
  
  (provide (all-defined))
  (define FILE-STORAGE-PATH     (make-parameter (build-path "/Users/jacobm/tmp/planet-files")))
  (define WEB-PACKAGES-ROOT     (make-parameter (build-path "/Users/jacobm/svn/plt/collects/web-server/default-web-root/htdocs/planet-packages/")))
  
  (define URL-ROOT              (make-parameter "http://localhost:8080"))
  (define DISPLAY-URL-ROOT      (make-parameter (string-append (URL-ROOT) "/servlets/planet2/display-servlet.ss")))
  (define ADD-URL-ROOT          (make-parameter (string-append (URL-ROOT) "/servlets/planet2/add-servlet.ss")))
  (define WEB-PACKAGES-URL-ROOT (make-parameter (string-append (URL-ROOT) "/planet-packages")))
  
  (define DEFAULT-REPOSITORY (make-parameter 2)) ; the repository to show by default; corresponds to 3xx


  
  
  
  )
