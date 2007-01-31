(module planet-server mzscheme

  (require
   (lib "contract.ss")
   (lib "planet-shared.ss" "planet" "private")
   "data-structures.ss"
   "db.ss")

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
    (if (legal-language? language-version)
        (let ([cache-pkg-list (get-matching-packages language-version
                                                     (car (pkg-spec-path pkg-spec))
                                                     (pkg-spec-name pkg-spec)
                                                     (pkg-spec-maj pkg-spec)
                                                     (pkg-spec-minor-lo pkg-spec)
                                                     (pkg-spec-minor-hi pkg-spec))])
          (if (pair? cache-pkg-list)
              (let* ([cache-pkg (car cache-pkg-list)]
                     [file (pkgversion-plt-path cache-pkg)])
                (if (file-exists? file)
                    (begin
                      (transmit-file cache-pkg file)
                      (proceed-k))
                    (begin
                      (transmit-failure pkg-spec 'not-found "Internal error: inconsistent server state")
                      (stop-k))))
              (begin
                (transmit-failure pkg-spec 'not-found "No package matched the specified criteria")
                (proceed-k))))
        (begin
          (transmit-failure pkg-spec 'bad-language (format "Unknown package language: ~s" language-version))
          (stop-k)))))
