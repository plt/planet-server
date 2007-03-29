(module planet-server mzscheme

  (require
   (lib "contract.ss")
   (all-except (lib "planet-shared.ss" "planet" "private") legal-language?)
   "data-structures.ss"
   "db.ss"
   (prefix old-server: "old-planet-server.ss"))

  (provide/contract
   [handle-one-request
    (string?
     pkg-spec?
     (pkgversion? path? . -> . any)
     (pkg-spec? symbol? string? . -> . any)
     (-> any)
     (-> any)
     . -> .
     any)])

  ;; ============================================================
  ;; BASIC SERVER MECHANISMS

  ;; handle-one-one-request : string,                             [language version for package]
  ;;                          pkg-spec,                           [package's specification]
  ;;                          (pkg nat nat path -> void),         [function to call to transmit a file]
  ;;                          (pkg-spec symbol string -> void),   [function to call to report a failure]
  ;;                          (-> void),                          [continuation to call if processing can continue]
  ;;                          (-> void)                           [continuation to call if processing must stop]
  ;; ->
  ;; void
  (define (handle-one-request language-version pkg-spec transmit-file transmit-failure proceed-k stop-k)
    (startup)
    (cond
      [(legal-language? language-version)
       (let-values ([(cache-pkg-list client-too-old?)
                     (get-matching-packages language-version
                                            (car (pkg-spec-path pkg-spec))
                                            (pkg-spec-name pkg-spec)
                                            (pkg-spec-maj pkg-spec)
                                            (pkg-spec-minor-lo pkg-spec)
                                            (pkg-spec-minor-hi pkg-spec))])
          (cond
            [(pair? cache-pkg-list)
             (let* ([cache-pkg (car cache-pkg-list)]
                    [file (pkgversion-plt-path cache-pkg)])
               (if (file-exists? file)
                   (begin
                     (transmit-file cache-pkg file)
                     (proceed-k))
                   (begin
                     (transmit-failure pkg-spec 'not-found "Internal error: inconsistent server state")
                     (stop-k))))]
            [else
             (begin
               (transmit-failure pkg-spec 
                                 'not-found
                                 (if client-too-old?
                                     "Your version of PLT Scheme is too old to run the specified package"
                                     "No package matched the specified criteria"))
               (proceed-k))]))]
      [(regexp-match #rx"^20.+" language-version)
       ;; hack to continue to support 20x series PLT Scheme even though the relation between
       ;; repositories and packages has changed. [It used to be that case that each repository
       ;; was an entirely separate universe, and a package could have two totally separate tracks
       ;; in two different repositories. This turned out to be a pain, because when you ported a
       ;; a program from 20x to 3xx you would always have to change the planet require lines, even
       ;; if the program otherwise worked fine. So, now a package belongs to multiple repositories
       ;; at a time; repositories are basically just a filter at this point.
       ;;
       ;; It's probably a better way to do things, but it also causes a backwards-compatibility
       ;; problem, since the new way of doing things is incompatible with existing version numbers
       ;; for packages that exist in both the 20x and 3xx repositories. So rather than rebuilding
       ;; the data model to allow for this sort of thing, I've just included a backwards compatibility
       ;; hack that redirects 20x clients to an instance of the old server. Essentially the world started
       ;; over at 300 but will continue forward without another break like this.
       (old-server:handle-one-request language-version pkg-spec transmit-file transmit-failure proceed-k stop-k)]
      [else
       (transmit-failure pkg-spec 'bad-language (format "Unknown package language: ~s" language-version))
       (stop-k)])))
