(module new-planet-server mzscheme
  
  (require "db.ss" "data-structures.ss")
  (require (lib "class.ss"))
  (require (lib "match.ss"))
  
  (define (string->core-version-number s)
    (let ((version (regexp-match #rx"([0-9]+)(?:\\.([0-9]+))?" s)))
      (unless version
        (error 'string->core-version-number "bogus" s))
      (let-values ([(maj min)
                    (match version
                      [`(,_ ,maj #f) (values (string->number maj) 0)]
                      [`(,_ ,maj ,min) (values (string->number maj) (string->number min))])])
        (number->string (+ (* maj 10000) min)))))
  
  )
