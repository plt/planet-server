(module configuration mzscheme
  
  (provide (all-defined))
  (define FILE-STORAGE-PATH     (make-parameter (build-path "/Users/jacobm/tmp/planet-files")))
  (define WEB-PACKAGES-ROOT     (make-parameter (build-path "/Users/jacobm/svn/plt/collects/web-server/default-web-root/htdocs/planet-packages/")))
  
  (define URL-ROOT              (make-parameter "http://localhost:8080"))
  (define DISPLAY-URL-ROOT      (make-parameter (string-append (URL-ROOT) "/servlets/planet/display-servlet.ss")))
  (define ADD-URL-ROOT          (make-parameter (string-append (URL-ROOT) "/servlets/planet/add-servlet.ss")))
  (define LOGOUT-PAGE           (make-parameter (string-append (URL-ROOT) "/servlets/planet/add-servlet.ss?mode=logout")))
  (define WEB-PACKAGES-URL-ROOT (make-parameter (string-append (URL-ROOT) "/planet-packages")))
  
  (define DEFAULT-REPOSITORY (make-parameter 2)) ; the repository to show by default; corresponds to 3xx

  ;; ===
  ;; bug-tracker configuration
  (define MANTIS-INTERFACE-COMMAND-LINE (make-parameter "/usr/local/bin/php -f /Library/WebServer/Documents/mantis/jacobs_test.php"))
  (define MANTIS-BUG-VIEW-URL (make-parameter (λ (bug-id) (format "http://localhost/mantis/view.php?id=~a" bug-id))))
  (define MANTIS-BUG-REPORT-PAGE-URL 
    (make-parameter 
     (λ (mantis-project-id) 
       (format "http://localhost/mantis/set_project.php?project_id=~a&ref=bug_report_page.php" mantis-project-id))))
  
  )