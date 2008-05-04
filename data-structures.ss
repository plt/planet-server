(module data-structures mzscheme

  (require (lib "struct.ss")
           (lib "list.ss")
           (lib "contract.ss"))

  (provide (struct user (id username realname email))
           (struct category (id name shortname packages))
           (struct package (id owner name blurb homepage versions bugtrack-id))
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
           (struct primary-file (name xexpr))
           (struct repository (id name client-lower-bound client-upper-bound urlname))
           (struct exn:user ())
           raise-user-error)
  

  (define-struct user (id       ; nat
                       username ; string
                       realname ; string
                       email    ; string
                       )
    (make-inspector))

  (define-struct category (id        ; nat
                           name      ; string
                           shortname ; symbol | #f
                           packages  ; listof package | #f
                           )
    (make-inspector))

  (define-struct package (id          ; nat
                          owner       ; string
                          name        ; string
                          blurb       ; (listof xexpr) | #f
                          homepage    ; string[url] | #f
                          versions    ; (ne-listof pkgversion)[sorted newest to oldest]
                                      ; #f [for a package-stub]
                          bugtrack-id ; nat : foreign key into mantis database [unused; included for future use]
                          )
    (make-inspector))

  (define-struct pkgversion (id package-id maj min
                             plt-path src-path
                             default-file doctxt
                             blurb ;; release notes
                             date-added
                             name
                             repositories   ;; listof nat [repository ids]
                             required-core
                             downloads)
    (make-inspector))

  (define-struct primary-file (name  ; string 
                               xexpr ; xexpr | #f
                               ) 
    (make-inspector))
  
  (define-struct repository (id                    ; natural-number 
                             name                  ; string
                             client-lower-bound    ; natural-number
                             client-upper-bound    ; natural-number
                             urlname               ; string
                             ) (make-inspector))

  ;; ============================================================
  ;; UTILITY

  (provide/contract
   [package->current-version
    (package? . -> . pkgversion?)]
   [package->old-versions
    (package? . -> . (listof pkgversion?))]
   [filter-package-for-repository
    (package? natural-number/c . -> . (union package? false/c))]
   [pv=? (->* (pkgversion?) (listof pkgversion?) (boolean?))]
   [pv<? (->* (pkgversion?) (listof pkgversion?) (boolean?))]
   [pv>? (->* (pkgversion?) (listof pkgversion?) (boolean?))])

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
  
  
  (define (collect-boolean fn elts)
    (cond
      [(null? elts) #t]
      [(null? (cdr elts)) #t]
      [else
       (and (fn (car elts) (cadr elts)) (collect-boolean fn (cdr elts)))]))
  
  (define (lift-binary-predicate f)
    (λ (a . bs) (collect-boolean f (cons a bs))))
  
  ;; pv=? : pv pv -> bool
  ;; determine if these two package versions represent the same thing
  (define pv=? 
    (lift-binary-predicate (λ (a b) (= (pkgversion-id a) (pkgversion-id b)))))
  
  ;; pv<? : pv pv -> bool
  ;; determine if the first pkg version represents a lower-numbered revision of the same
  ;; package as the second pkg version
  (define pv<?
    (lift-binary-predicate
     (λ (a b)
       (and (= (pkgversion-package-id a) (pkgversion-package-id b))
            (or (< (pkgversion-maj a) (pkgversion-maj b))
                (and 
                 (= (pkgversion-maj a) (pkgversion-maj b))
                 (< (pkgversion-min a) (pkgversion-min b))))))))
  
  (define (pv>? a . bs)
    (apply pv<? (reverse (cons a bs))))
  
   
  
  
  ;; ============================================================
  ;; user error reporting
  
  (define-struct (exn:user exn) () (make-inspector))
  (define (raise-user-error msg)
    (raise (make-exn:user msg (current-continuation-marks))))

  )
