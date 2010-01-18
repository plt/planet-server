(module configuration mzscheme
  (require (lib "url.ss" "net"))
  
  (provide (all-defined))
  (define FILE-STORAGE-PATH     (make-parameter (build-path "/home/wwwplanet/planet/archives")))
  (define STATIC-CONTENT-ROOT   (make-parameter (build-path "/home/wwwplanet/webroot/htdocs/")))
  (define WEB-PACKAGES-ROOT     (make-parameter (build-path (STATIC-CONTENT-ROOT) "package-source")))
  (define AUTOINSTALLERS-ROOT   (make-parameter (build-path (STATIC-CONTENT-ROOT) "autoinstallers")))
  
  (define EXTERNAL-URL-ROOT     (make-parameter (string->url "http://planet.plt-scheme.org/")))
  (define URL-ROOT              (make-parameter "/"))
  (define URL-SERVLET-BASE      (make-parameter "/servlets"))
  (define DISPLAY-URL-ROOT      (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/display.ss")))
  (define ADD-URL-ROOT          (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/add.ss")))
  (define LOGOUT-PAGE           (make-parameter (string-append #;(URL-ROOT) #;(URL-SERVLET-BASE) "/add.ss?mode=logout")))
  (define WEB-PACKAGES-URL-ROOT (make-parameter (string-append #;(URL-ROOT) "/package-source")))
  
  (define DATABASE-CONNECT-ARGUMENTS (make-parameter '(#:database "planet" #:user "jacobm" #:password "matrix")))
  (define DEFAULT-REPOSITORY (make-parameter 3)) ; the repository to show by default; corresponds to 4.x
  
  (define DISPLAY-ERRORS-OVER-WEB? (make-parameter #t))
  (define SEND-EMAILS? (make-parameter #t))
  (define ANNOUNCE-NEW-PACKAGES? (make-parameter #t))
  
  ;; email announcements
  (define PLANET-FROM-ADDRESS (make-parameter "PLaneT <planet@plt-scheme.org>"))
  (define TO-ADDRESSES (make-parameter '("plt-planet-announce@list.cs.brown.edu")))
  (define MAIL-SUBSCRIBE-URL 
    (make-parameter 
     (string->url 
      "http://list.cs.brown.edu/mailman/listinfo/plt-planet-announce")))
  (define NEW-MAIL-SUBJECT
    (make-parameter (lambda (owner pkgname reps) (format "New PLaneT package: ~a/~a for ~a" owner pkgname reps))))
  (define UPDATED-MAIL-SUBJECT
    (make-parameter (lambda (owner pkgname reps) (format "Updated PLaneT package: ~a/~a for ~a" owner pkgname reps))))
  (define NEW-PACKAGE-ANNOUNCEMENT-TEMPLATE (make-parameter "newpkg.mzpp"))
  (define UPDATED-PACKAGE-ANNOUNCEMENT-TEMPLATE (make-parameter "updatedpkg.mzpp"))
    
  ;; rss feed
  (define NUM-RSS-ITEMS (make-parameter 10))
  (define RSS-FILE-NAME (make-parameter "planet.rss"))
  (define REPOSITORIES-STATIC-CONTENT-URL-ROOT URL-ROOT)
  (define REPOSITORIES-STATIC-CONTENT-FILE-ROOT STATIC-CONTENT-ROOT)

  ;; trac
  (define TRAC-PATH "/home/wwwplanet/Trac-environment/")
  (define TRAC-PASSWORDS (string-append TRAC-PATH "users.txt"))
  (define TRAC-PASSWORDS-TMP (string-append TRAC-PATH "temp.tmp"))
  (define TRAC-PASSWORD-LOCKFILE (string-append TRAC-PATH "lockfile.lock"))

  (define PYTHON-ROOT "/home/wwwplanet/python2.6/")
  (define PYTHON (string-append PYTHON-ROOT "bin/python"))
  (define TRAC-ADMIN (string-append PYTHON-ROOT "bin/trac-admin"))
  (define USERS.PY "/home/wwwplanet/svn/iplt/planet/tracplanet/users.py")
  (define TRAC-LOCAL-TICKET-URL "http://planet.plt-scheme.org/trac/ticket/~a?format=tab")
  (define TRAC-LOCAL-TICKETS-URL "http://planet.plt-scheme.org/trac/query?format=tab")
  (define TRAC-HOST "planet.plt-scheme.org")

  )

;; note to self: find-str 8080
