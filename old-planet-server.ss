#| old-planet-server : implements the old lookup mechanism for packages. Called by planet-server.ss when the 
 requester's core version is in the 20x series. |#
(module old-planet-server mzscheme
  
  (require 
   (lib "planet-shared.ss" "planet" "private")
   (only (lib "config.ss" "planet") CACHE-DIR))
  
  (provide handle-one-request)
  
  (define PLANET-SERVER-REPOSITORY (make-parameter (build-path "/" "local" "planet" "20x")))
  
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
    (if (legal-language? language-version)
        (let* ([repository (build-path 
                            (PLANET-SERVER-REPOSITORY) 
                            (language-version->repository language-version))]
               [cache-pkg (parameterize ((CACHE-DIR repository))
                            (lookup-package pkg-spec))])
          (if cache-pkg
              (let* ([path (pkg-path cache-pkg)]
                     [maj (pkg-maj cache-pkg)]
                     [min (pkg-min cache-pkg)]
                     [file (build-path path (pkg-spec-name pkg-spec))])
                (if (file-exists? file)
                    (begin
                      (transmit-file cache-pkg maj min file)
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