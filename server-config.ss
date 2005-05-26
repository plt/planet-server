(module server-config mzscheme
  
  (require (lib "planet-shared.ss" "planet" "private")
           (lib "etc.ss")
           (lib "url.ss" "net"))
  
  (define-parameters 
    
    ;; repository
    (PLANET-SERVER-REPOSITORY (build-path (this-expression-source-directory) "repository"))
    (PLANET-SERVER-PORT       270)
    (METAINFO-FILE            "info.ss")
    
    ;; logging
    (PLANET-LOG-DIR           (this-expression-source-directory))
    (PLANET-ERROR-LOG         (build-path (PLANET-LOG-DIR) "ERRORS"))
    (PLANET-CONNECT-LOG       (build-path (PLANET-LOG-DIR) "LOG"))
  
    ;; email
    (PLANET-FROM-ADDRESS "PLaneT <planet@plt-scheme.org>")
    (TO-ADDRESSES '("planet-announce@mailman.cs.uchicago.edu"))
    (MAIL-SUBSCRIBE-URL (string->url "http://mailman.cs.uchicago.edu/mailman/listinfo/planet-announce"))
    (NEW-MAIL-SUBJECT
     (lambda (pkgname reps) (format "New PLaneT package: ~a for ~a" pkgname reps)))
    (SINGLE-REPOSITORY-MAIL-TEMPLATE "newpkg.mzpp")
    (MULTI-REPOSITORY-MAIL-TEMPLATE "newpkg2.mzpp")
    
    
    ;; rss feed
    (NUM-RSS-ITEMS 10)
    (RSS-FILE-NAME "planet.rss")
    
    ;; web page
    (PORT                     8000)
    (PLANET-WEB-HOST          "planet.plt-scheme.org")
    (PLANET-WEB-PATH          '())
    (WEBROOT                  "/home/jacobm/html/planet-mockup/")
    (WEB-PAGE-FILE            "index.html")
    (NEWEST-PAGE-FILE         "newest.html")
    (DOCS-DIR                 "docs")))
