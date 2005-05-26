(module test mzscheme
  (require "html2text.ss")
  
  (define-syntax should-be
  (syntax-rules ()
    ((_ test-id value expression)
     (let ((return-value expression))
         (if (not (equal? return-value value))
           (for-each (lambda (v) (display v))
                     `("Failure: " test-id ", expected '"
                     value "', got '" ,return-value "'." #\newline))
           (for-each (lambda (v) (display v))
                     '("Passed: " test-id #\newline)))))))
  
  ;; much more testing to be done ...
  (should-be 1.1 
             (html-expr->text `(div " " " "))
             "")
  
  (should-be 1.2
             (html-expr->text `(div "a" "b"))
             "a b")
  
  (should-be 1.3
             (html-expr->text `(div "a           " "b"))
             "a b")
  
  (should-be 1.4
             (html-expr->text `(div "hi" (br) "there"))
             "hi\nthere")

  (should-be 1.5
             (html-expr->text `(div (p "hi") (p "there")))
             "\nhi\n\nthere\n")
  )
