(module run-tests mzscheme
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  (require "planet-server-test.ss")

  (test/text-ui all-tests))
