#lang scheme
;; note that if the PLaneT server is upgraded, this file
;; will end up requiring itself (but in that case, this file
;; can probably just be deleted, since requiring doc-search.rkt
;; will end up getting doc-search.ss anyways). But do test this.
;; Here's one way to test it: visit this url:
;;  http://localhost:19173/package-source/neil/scribble-emacs.plt/1/2/planet-docs/scribble-emacs/index.html:
(require "doc-search.ss") 
(provide (all-from-out "doc-search.ss"))