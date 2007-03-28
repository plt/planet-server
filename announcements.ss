(module announcements mzscheme
  (require (lib "xml.ss" "xml")
           (lib "url.ss" "net"))
  (require "configuration.ss"
           "data-structures.ss"
           "db.ss")
  
  ;; ============================================================
  ;; RSS
  
  ;; repository -> url
  ;; gets the base url for all static content relating to the given repository [currently just rss feed]
  (define (repository->base-url rep)
    (combine-url/relative (REPOSITORIES-STATIC-CONTENT-URL-ROOT) (repository-urlname rep)))
  
  ;; repository -> url
  ;; gets the url for the repository's rss feed
  (define (repository->rss-url rep)
    (combine-url/relative (repository->base-url rep) (RSS-FILE-NAME)))
  
  ;; repository->rss-path : repository -> path[file]
  ;; gets the filename of the rss feed for the given repository
  (define (repository->rss-path rep)
    (build-path (REPOSITORIES-STATIC-CONTENT-FILE-ROOT) (repository-urlname rep) (RSS-FILE-NAME)))
  
  ;; pkg->url : package -> string[url]
  ;; probably a bad idea
  (define (pkg->url pkg)
    (combine-url/relative 
     (EXTERNAL-URL-ROOT)
     (format "/users/~a/~a/" (package-owner pkg) (package-name pkg))))
  
  ;; rebuild-rss-feed : repository? -> void
  ;; as a side effect, rebuilds the rss file for the given repository id
  (define (rebuild-rss-feed rep)
    (let* ([new-rss-xexpr
            `(rss 
              ((version "2.0"))
              (channel
               (title "New PLaneT Packages")
               (link ,(url->string (EXTERNAL-URL-ROOT)))
               (description "The newest packages available from PLaneT")
               (language "en-us")
               ,@(map
                  (lambda (pkg)
                    (let ([pkgversion (package->current-version pkg)])
                      `(item (title ,(package-name pkg))
                             (description 
                              ,(escape-xexprs-as-xml-string 
                                (append 
                                 (package-blurb pkg)
                                 (or (pkgversion-blurb pkgversion) '()))))
                             (link ,(url->string (pkg->url pkg))))))
                  (get-n-most-recent-packages (NUM-RSS-ITEMS) rep))))]
           [file-to-write (repository->rss-path rep)]
           [o (open-output-file file-to-write 'truncate)])
      (display "<?xml version=\"1.0\"?>\n" o)
      (write-xml/content (xexpr->xml new-rss-xexpr) o)
      (close-output-port o)))
  
  ;; escape-xexprs-as-xml-string : (listof xexpr) -> string
  (define (escape-xexprs-as-xml-string xprs)
    (let ((op (open-output-string)))
      (for-each
       (lambda (x) (write-xml/content (xexpr->xml x) op))
       xprs)
      (get-output-string op)))

  #|
  ;; ============================================================
  ;; EMAIL
  

  
  (require 
   "repository-types.ss"
   "server-config.ss"
   "html2text.ss"
   
   (lib "contract.ss")
   (lib "sendmail.ss" "net")
   (lib "url.ss" "net")
   (prefix mzpp: (lib "mzpp.ss" "preprocessor"))
   (prefix srfi13: (lib "13.ss" "srfi")))
  
  (provide/contract 
   (update-email-list notifier?))
  
  
  ;; instantiate-template : path[file] (listof (list symbol TST)) output-port -> void
  ;; evaluates file, which is in mzpp format, in a namespace in which the given symbols 
  ;; are bound to their associated values, and prints the result to port
  (define (instantiate-template file template-vars/vals port)
    (parameterize ((current-namespace (make-namespace))
                   (current-output-port port))
      (for-each
       (lambda (x) (namespace-set-variable-value! (car x) (cadr x)))
       template-vars/vals)
      (let ((ip (open-input-file file)))
        (mzpp:preprocess ip)
        (close-input-port ip))))
  
  ;; update-email-list : notifier
  ;; mails the email list with the new package
  (define (update-email-list pkgs)
    (if (null? pkgs)
        (void)
        (update-email-list/internal pkgs)))
    
  (define (update-email-list/internal pkgs)
    (let ((pkg (car pkgs)))
      (let* ([blurb (html-expr->text `(div ,@(pkg->blurb pkg)))]
             [repositories-as-string
              (srfi13:string-join
               (map
                (lambda (p) (repository-name (installed-package-repository p)))
                pkgs)
               ", ")]
             [op (send-mail-message/port 
                  (PLANET-FROM-ADDRESS)
                  ((NEW-MAIL-SUBJECT) (package-name pkg) repositories-as-string)
                  (TO-ADDRESSES)
                  '()
                  '())]
             [shared-template-fields
              (list
               (list 'name          (package-name pkg))
               (list 'owner         (package-owner pkg))
               (list 'blurb         blurb)
               (list 'doc-url       (url->string (pkg->doc-url pkg)))
               (list 'url           (url->string (pkg->url pkg))))])
        (if (null? (cdr pkgs)) ;; pkg is installed in exactly 1 repository
            (instantiate-template (SINGLE-REPOSITORY-MAIL-TEMPLATE)
                                  (list* 
                                   (list 'major-version (package-maj pkg))
                                   (list 'minor-version (package-min pkg))
                                   shared-template-fields)
                                  op)
            (instantiate-template (MULTI-REPOSITORY-MAIL-TEMPLATE)
                                  (list*
                                   (list 'installations
                                         (map (lambda (p) (list 
                                                           (repository-name (installed-package-repository p))
                                                           (package-maj p)
                                                           (package-min p)))
                                              pkgs))
                                   shared-template-fields)
                                  op))
        (close-output-port op) ;; causes the mail to actually get sent
        )))

  
  |#
  
  
  )