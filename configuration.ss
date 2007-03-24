(module configuration mzscheme
  
  (provide (all-defined))
  (define FILE-STORAGE-PATH     (make-parameter (build-path "/Users/jacobm/tmp/planet-files")))
  (define WEB-PACKAGES-ROOT     (make-parameter (build-path "/Library/WebServer/Documents/package-source/")))
  
  (define URL-ROOT              (make-parameter "/"))
  (define URL-SERVLET-BASE      (make-parameter "/servlets"))
  (define DISPLAY-URL-ROOT      (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/display.ss")))
  (define ADD-URL-ROOT          (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/add.ss")))
  (define LOGOUT-PAGE           (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/add.ss?mode=logout")))
  (define WEB-PACKAGES-URL-ROOT (make-parameter (string-append #;(URL-ROOT) "/package-source/")))
  
  (define DEFAULT-REPOSITORY (make-parameter 2)) ; the repository to show by default; corresponds to 3xx
  
  (define DISPLAY-ERRORS-OVER-WEB? (make-parameter #t))
  
  )
