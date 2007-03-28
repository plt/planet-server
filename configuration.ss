(module configuration mzscheme
  (require (lib "url.ss" "net"))
  
  (provide (all-defined))
  (define FILE-STORAGE-PATH     (make-parameter (build-path "/local/planet/archives")))
  (define STATIC-CONTENT-ROOT   (make-parameter (build-path "/local/webroot/htdocs/")))
  (define WEB-PACKAGES-ROOT     (make-parameter (build-path (STATIC-CONTENT-ROOT) "package-source")))
  
  (define EXTERNAL-URL-ROOT     (make-parameter (string->url "http://planet.plt-scheme.org/")))
  (define URL-ROOT              (make-parameter "/"))
  (define URL-SERVLET-BASE      (make-parameter "/servlets"))
  (define DISPLAY-URL-ROOT      (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/display.ss")))
  (define ADD-URL-ROOT          (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/add.ss")))
  (define LOGOUT-PAGE           (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/add.ss?mode=logout")))
  (define WEB-PACKAGES-URL-ROOT (make-parameter (string-append #;(URL-ROOT) "/package-source/")))
  
  (define DEFAULT-REPOSITORY (make-parameter 2)) ; the repository to show by default; corresponds to 3xx
  
  (define DISPLAY-ERRORS-OVER-WEB? (make-parameter #t))
  (define SEND-EMAILS? (make-parameter #t))
  (define ANNOUNCE-NEW-PACKAGES? (make-parameter #t))
  
  ;; email announcements
  (define PLANET-FROM-ADDRESS (make-parameter "PLaneT <planet@plt-scheme.org>"))
  (define TO-ADDRESSES (make-parameter '("jacobm@gmail.com")))
  (define MAIL-SUBSCRIBE-URL (make-parameter (string->url "http://mailman.cs.uchicago.edu/mailman/listinfo/planet-announce")))
  (define NEW-MAIL-SUBJECT
    (make-parameter (lambda (pkgname reps) (format "New PLaneT package: ~a for ~a" pkgname reps))))
  (define NEW-PACKAGE-ANNOUNCEMENT-TEMPLATE (make-parameter "newpkg.mzpp"))
  (define UPDATED-PACKAGE-ANNOUNCEMENT-TEMPLATE (make-parameter "updatedpkg.mzpp"))
    
  ;; rss feed
  (define NUM-RSS-ITEMS (make-parameter 10))
  (define RSS-FILE-NAME (make-parameter "planet.rss"))
  (define REPOSITORIES-STATIC-CONTENT-URL-ROOT URL-ROOT)
  (define REPOSITORIES-STATIC-CONTENT-FILE-ROOT STATIC-CONTENT-ROOT)

  )
