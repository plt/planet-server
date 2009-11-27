#lang scheme/base
(require scheme/tcp
         scheme/unit
	 scheme/date
         net/tcp-sig)
(require web-server/configuration/configuration-table
         web-server/web-server-unit
         web-server/web-server-sig
         web-server/web-config-unit
         web-server/web-config-sig
         (only-in web-server/web-server do-not-return))
(require "top-bug-closers.ss")

(file-stream-buffer-mode (current-output-port) 'line)
(file-stream-buffer-mode (current-error-port) 'line)

(define configuration@
  (configuration-table->web-config@
   (string->path "/home/wwwplanet/webroot/configuration-table")))

(define-compound-unit launch@
  (import (T : tcp^))
  (export S)
  (link 
   [((C : web-config^)) configuration@]
   [((S : web-server^)) web-server@ T C]))

(define-values/invoke-unit
  launch@
  (import tcp^)
  (export web-server^))

(printf "PLaneT starting up at ~a (~s)\n" 
	(date->string (seconds->date (current-seconds)) #t)
	(current-seconds))

(void (spawn-bug-closer-thread))

(void (serve))

(do-not-return)

