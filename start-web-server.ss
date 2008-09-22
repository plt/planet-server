#lang scheme/base
(require scheme/tcp
         scheme/unit
         net/tcp-sig)
(require web-server/configuration/configuration-table
         web-server/web-server-unit
         web-server/web-server-sig
         web-server/web-config-unit
         web-server/web-config-sig
         (only-in web-server/web-server do-not-return))
(require "top-bug-closers.ss")

(define configuration@
  (configuration-table->web-config@
   (string->path "/local/webroot/configuration-table")))

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

(spawn-bug-closer-thread)

(void (serve))

(do-not-return)

