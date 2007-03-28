(module announcements mzscheme
  (require (lib "xml.ss" "xml")
           (lib "url.ss" "net")
           (lib "contract.ss")
           (lib "sendmail.ss" "net")
           (prefix mzpp: (lib "mzpp.ss" "preprocessor"))
           (prefix srfi13: (lib "13.ss" "srfi")))
  (require "configuration.ss"
           "data-structures.ss"
           "db.ss"
           "html.ss"
           "html2text.ss")

  (provide/contract
   [rebuild-rss-feed (-> repository? any)]
   [update-email-list (package? . -> . any)])
  
  ;; ============================================================
  ;; RSS
  
  ;; repository -> url
  ;; gets the url for the repository's rss feed
  (define (repository->rss-url rep)
    (combine-url/relative (repository->base-url rep) (RSS-FILE-NAME)))
  
  ;; repository->rss-path : repository -> path[file]
  ;; gets the filename of the rss feed for the given repository
  (define (repository->rss-path rep)
    (build-path (REPOSITORIES-STATIC-CONTENT-FILE-ROOT) (repository-urlname rep) (RSS-FILE-NAME)))
  
  
  
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
                                 (or (package-blurb pkg) '())
                                 (or (pkgversion-blurb pkgversion) '()))))
                             (link ,(url->string (package->link/base pkg))))))
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

  ;; ============================================================
  ;; EMAIL
  
  
  
  
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
  (define (update-email-list pkg)
    (let* ([pkgversion (package->current-version pkg)]
           [blurb (html-expr->text 
                   `(div
                     ,@(if (package-blurb pkg)
                           `((p "Package Description")
                             ,@(package-blurb pkg))
                           '())
                     ,@(if (pkgversion-blurb pkgversion)
                           `((p "Release Notes")
                             ,@(pkgversion-blurb pkgversion))
                           '())))]
           [repositories-as-string
            (srfi13:string-join
             (map repository-name (pkgversion-repositories pkgversion))
             ", ")]
           [op (send-mail-message/port 
                (PLANET-FROM-ADDRESS)
                ((NEW-MAIL-SUBJECT) (package-name pkg) repositories-as-string)
                (TO-ADDRESSES)
                '()
                '())]
           [template-fields
            (list
             (list 'name          (package-name pkg))
             (list 'owner         (package-owner pkg))
             (list 'major-version (pkgversion-maj pkgversion))
             (list 'minor-version (pkgversion-min pkgversion))
             (list 'blurb         blurb)
             (list 'doc-url       (url->string (pkgversion->docs-link pkg)))
             (list 'url           (url->string (package->link/base pkg))))])
      (begin
        (cond
          [(new-package? pkgversion)
           (instantiate-template (NEW-PACKAGE-ANNOUNCEMENT-TEMPLATE) template-fields op)]
          [else
           (instantiate-template (UPDATED-PACKAGE-ANNOUNCEMENT-TEMPLATE) template-fields op)])
        (close-output-port op) ;; causes the mail to actually get sent
        )))
  
  (define (new-package? pkgversion)
    (and (= (pkgversion-maj pkgversion) 1)
         (= (pkgversion-min pkgversion) 0)))
  
  )