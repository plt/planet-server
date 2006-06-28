(module logging mzscheme
  ;; logging.ss -- provides logging facilities for the planet server
  
  (require "server-config.ss")
  (provide log-download log-error)
  ;; ============================================================
  
  ; log-download : string string string nat nat -> void
  ; logs the given download
  (define (log-download ip-addr owner pkg-name v-maj v-min)
    (record (list ip-addr (current-seconds) owner pkg-name v-maj v-min) (PLANET-CONNECT-LOG)))
  
  ; log-error : string symbol tst ... -> void
  ; logs the given error. The extras should be information relevant to the given error type.
  (define (log-error ip-addr err-type . extras)
    (record (list* ip-addr (current-seconds) err-type extras) (PLANET-ERROR-LOG)))
  
  (define (record msg file)
    (let ((op (open-output-file file 'append)))
      (fprintf op "~s\n" msg)
      (flush-output op)
      (close-output-port op)))
  
  
  )