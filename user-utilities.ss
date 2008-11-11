(module user-utilities mzscheme

  (require
   web-server/servlet/bindings
   web-server/private/request-structs
   (lib "contract.ss")
   "cookie-monster.ss" "db.ss" "data-structures.ss")

  (provide/contract
   [logged-in-user (-> request? (union user? false/c))])

  ;; libraries to determine and manage the logged-in user

  (define (logged-in-user r)
    ;; extract the logged-in user that corresponds to this request, if any
    (with-handlers ([exn:fail? (λ (e) 
                                 ;(error 'logged-in-user "oops! I got: ~a\n" (exn-message e))
                                 
                                 #f)])
      (let* ([cs (request-cookies r)]
             [username (extract-binding/single 'username cs)]
             [passcode (extract-binding/single 'passcode cs)])
        (get-logged-in-user-from-passcode username passcode)))))
