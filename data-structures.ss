(module data-structures mzscheme

  (require (lib "struct.ss")
           (lib "list.ss")
           (lib "contract.ss"))

  (provide (struct user (id username realname))
           (struct category (id name shortname packages))
           (struct package (id owner name blurb versions))
           (struct pkgversion (id
                               package-id
                               maj min
                               plt-path src-path
                               default-file doctxt
                               blurb
                               date-added
                               name
                               repositories
                               required-core
                               downloads))
           (struct repository (id name client-lower-bound client-upper-bound)))

  (define-struct user (id       ; nat
                       username ; string
                       realname ; string
                       )
    (make-inspector))

  (define-struct category (id        ; nat
                           name      ; string
                           shortname ; symbol | #f
                           packages  ; listof package | #f
                           )
    (make-inspector))

  (define-struct package (id         ; nat
                          owner      ; string
                          name       ; string
                          blurb      ; (listof xexpr)
                          versions   ; (ne-listof pkgversion)[sorted newest to oldest]
                                     ; #f [for a package-stub]
                          )
    (make-inspector))

  (define-struct pkgversion (id package-id maj min
                             plt-path src-path
                             default-file doctxt
                             blurb ;; release notes
                             date-added
                             name
                             repositories
                             required-core
                             downloads)
    (make-inspector))

  (define-struct repository (id name client-lower-bound client-upper-bound))

  ;; ============================================================
  ;; UTILITY

  (provide/contract
   [package->current-version
    (package? . -> . pkgversion?)]
   [package->old-versions
    (package? . -> . (listof pkgversion?))]
   [filter-package-for-repository
    (package? natural-number/c . -> . (union package? false/c))])

  (define (package-stub? pkg)
    (and (package? pkg) (not (package-versions pkg))))

  ;; package->current-version : package -> pkgversion
  ;; gets the most recent version of the given package
  (define (package->current-version pkg)
    (car (package-versions pkg)))

  ;; package->old-versions : package -> (listof pkgversion)
  ;; gets the historical versions of the given package,
  ;; sorted newest to oldest [excluding the current version]
  (define (package->old-versions pkg)
    (cdr (package-versions pkg)))

  (define (filter-package-for-repository pkg n)
    (let ([allowed-versions (filter (pkgversion-in-repository? n) (package-versions pkg))])
      (if (null? allowed-versions)
          #f
          (copy-struct package pkg (package-versions allowed-versions)))))

  ;; pkgversion-in-repository? : nat -> pkgversion -> boolean
  ;; determines if pv is in repository n
  (define ((pkgversion-in-repository? n) pv) (memv n (pkgversion-repositories pv)))

  )
