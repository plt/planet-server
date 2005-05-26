(module email mzscheme

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
        ))))