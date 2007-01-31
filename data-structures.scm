(module data-structures mzscheme

  (define-struct package (id owner name versions) (make-inspector))
  (define-struct pkgversion (id maj min
                             plt-path src-path
                             default-file doctxt
                             pkg-blurb
                             release-blurb
                             date-added
                             name
                             required-core)
    (make-inspector))

  (provide (struct package (id owner name versions))
           (struct pkgversion (id maj min
                               plt-path src-path
                               default-file doctxt
                               pkg-blurb
                               release-blurb
                               date-added
                               name
                               required-core))))
