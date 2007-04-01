(module log2dls mzscheme
  (require (lib "port.ss")
           (lib "match.ss")
           (lib "list.ss")
           (lib "date.ss"))
  
  (define (read-all ip)
    (let-values ([(pipe-in pipe-out) (make-pipe)])
      (thread 
       (λ ()
         (display #\( pipe-out)
         (copy-port ip pipe-out)
         (display #\) pipe-out)))
      (begin0
        (read pipe-in)
        (close-input-port pipe-in)
        (close-output-port pipe-out))))
  
  ;; ============================================================
  ;; general processing 
  
  (define (for-each-log-entry fn file)
    (for-each (handle-one fn) (call-with-input-file file read-all)))
  
  (define ((handle-one action) line)
    (define (do owner pkg maj min ip time)
      (action (list owner pkg maj min) (list ip time)))
    (match line
      [`(,ip-str ,path-str ,pkg ,maj ,min)
        (let-values ([(rep owner) (path->owner/rep path-str)])
          (when (string=? rep "300")
            (do owner pkg maj min ip-str #f)))]
      [`(,ip-str ,seconds ,path-str ,pkg ,maj ,min)
        (let-values ([(rep owner) (path->owner/rep path-str)])
          (when (string=? rep "300")
            (do owner pkg maj min ip-str seconds)))]))
  
  (define (path->owner/rep p)
    (let ([ans (regexp-match #rx"^/root/planet/rep/([^/]+)/([^/]+)/" p)])
      (unless ans
        (error 'path->owner/rep "non-path string: " p))
      (apply values (cdr ans))))
        
  (define (hash-table-add! ht k v)
    (hash-table-put! ht k (cons v (hash-table-get ht k (λ () '())))))
  
  ;; ============================================================
  ;; functions useful for examining old-style files
  
  (define (read-to-ht file)
    (define ht (make-hash-table 'equal))
    (for-each-log-entry (λ (pkg item) (hash-table-add! ht pkg item)) file)
    ht)
  
  (define (get-tabulation h)
    (sort (hash-table-map h (λ (k v) (list k (length v)))) (λ (a b) (< (cadr a) (cadr b)))))
  
  (define (retab tabs)
    (define ht (make-hash-table 'equal))
    (for-each 
     (λ (i) 
       (let ([k (car i)]
             [v (cadr i)])
         (hash-table-add! ht (list (car k) (cadr k)) v)))
     tabs)
    (sort (hash-table-map ht (λ (k v) (list k (apply + v)))) (λ (a b) (> (cadr a) (cadr b)))))
  
  ;; ============================================================
  ;; functions useful for importing old-style to new-style logs
  
  (define (format-date-to-sql secs)
    (parameterize ([date-display-format 'iso-8601])
      (date->string (seconds->date secs) #t)))
  )