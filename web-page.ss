(module web-page mzscheme
  
  (require (lib "util.ss" "planet")
           "planet-getinfo.ss"
           "server-config.ss"
           (file "../web/common/layout.ss")
           (lib "xml.ss" "xml")
           (lib "list.ss")
           "repository-info.ss")
  
  (provide build-web-page-file)
  
  (define (build-web-page-file webroot)
    (parameterize ((current-write-page 
                    (lambda (x y) 
                      (let ((out (open-output-file
                                  (build-path webroot (WEB-PAGE-FILE))
                                  'replace)))
                        (write-xml/content (xexpr->xml y) out)
                        (close-output-port out)))))
      (write-tall-page 
       "http://planet.plt-scheme.org/"
       "PLaneT Package Repository"
       (generate-web-page webroot)
       (list (make-hubs-panel #f #f)))))
  
  ;; generate-web-page : -> listof xexpr[xhtml]
  ;; makes the body of a web page telling all currently-available packages
  (define (generate-web-page webroot)
    (let ((tree (archive-as-tree webroot)))
      (list*
       `(div 
         ((class "description"))
         (p (strong "PLaneT") " is PLT Scheme's centralized package distribution system. Here you"
            " will find user-contributed Scheme packages along with instructions for using them.")
         (p "The packages on this site are user-contributed and not part of PLT Scheme. Be aware "
            "that when you download one of them for use in your programs, you are installing "
            "software on your computer that could deliberately or accidentally harm your system. "
            "Do not require from PLaneT any packages you do not trust.")
         (p "For more about how to use PLaneT and for instructions on turning your own code into"
            " packages, look up PLaneT in the DrScheme Help Desk."))
       (make-tall-page-section "Available Packages")
       (tree->table tree)
       (make-tall-page-section "Available Packages: Detail")
       (tree->xexprs tree))))
  
  (define pkg->anchor package-name)
  
  ;; tree->table : tree -> xexpr[html table]
  ;; builds a summary table of all the packages available.
  (define (tree->table tree)
    (define (owner->table-rows owner) (map package->table-row (owner-packages owner)))
    (define (package->table-row pkg)
      `(tr ((bgcolor "#ddddff")) 
           (td ((valign "top")) nbsp (a ((href ,(format "#~a" (pkg->anchor pkg)))) ,(package-name pkg)))
           (td ((valign "top")) ,@(package-blurb pkg))))
    
    `(table ((width "100%"))
            (tbody
             ,@(apply append (map owner->table-rows tree)))))
      
  
  (define (tree->xexprs tree)
    
    (define (owner->html owner)
      `(div 
        ((class "owner"))
        (h2 ,(owner-name owner))
        ,@(map package->html (owner-packages owner))))
    
    (define (package->html pkg)
      `(div ((class "package")
             (style "background-color: #f3f4ff; padding-left: 10px; margin-left: 10px; margin-right: 30px;"))
            (a ((name ,(pkg->anchor pkg))))
            (h3 ,(package-name pkg))
            (div ((class "latestVersion"))
                 ,(if (package-doc.txt pkg)
                      `(a ((href ,(package-doc.txt pkg)))
                          "documentation")
                      `(span ((class "noDocs")) "[no documentation available]"))
                 nbsp "-" nbsp
                 "latest version: " ,(format "~a.~a" (package-maj pkg) (package-min pkg)))
            (br)
            (tt ,(format "(require (planet ~s (~s ~s ~s ~s)))" 
                         (package-primary-file pkg)
                         (package-owner-name pkg)
                         (package-name pkg) 
                         (package-maj pkg)
                         (package-min pkg)))
            (p ,@(package-blurb pkg))))

      (map owner->html tree))
  

  )
