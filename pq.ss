(module pq mzscheme
  (require (lib "foreign.ss"))
  (unsafe!)
  
  
  (define pq (ffi-lib "libpq.so"))
  
  ;; ============================================================
  ;; mappings for types defined by postgres
  (define-cpointer-type _pgconn)
  (define-cpointer-type _pgresult)
  (define _oid _int)
  
  (define _conn-status-type
    (_enum
     (list
      'CONNECTION_OK
      'CONNECTION_BAD
      
      ;; Non-blocking mode only below here
      ;; The existence of these should never be relied upon - they should
      ;; only be used for user feedback or similar purposes.
      'CONNECTION_STARTED            ; Waiting for connection to be made.
      'CONNECTION_MADE               ; Connection OK; waiting to send.
      'CONNECTION_AWAITING_RESPONSE  ; Waiting for a response from the postmaster.
      'CONNECTION_AUTH_OK            ; Received authentication; waiting for backend startup.
      'CONNECTION_SETENV             ; Negotiating environment.
      'CONNECTION_SSL_STARTUP        ; Negotiating SSL.
      'CONNECTION_NEEDED             ; Internal state: connect() needed
      )))
 
  (define _polling-status-type
    (_enum
     '(PGRES_POLLING_FAILED = 0
       PGRES_POLLING_READING    ; These two indicate that one may
       PGRES_POLLING_WRITING    ; use select before polling again.
       PGRES_POLLING_OK
       PGRES_POLLING_ACTIVE     ; unused; keep for awhile for backwards compatibility
       )))

  (define _exec-status-type
    (_enum
     '(PGRES_EMPTY_QUERY = 0  ; empty query string was executed
       PGRES_COMMAND_OK       ; a query command that doesn't return
                              ;   anything was executed properly by the
                              ;   backend
       PGRES_TUPLES_OK        ; a query command that returns tuples was
                              ;   executed properly by the backend,
                              ;   PGresult contains the result tuples
       PGRES_COPY_OUT         ; Copy Out data transfer in progress
       PGRES_COPY_IN          ; Copy In data transfer in progress
       PGRES_BAD_RESPONSE     ; an unexpected response was recv'd from
                              ;   the backend
       PGRES_NONFATAL_ERROR   ; notice or warning message
       PGRES_FATAL_ERROR      ; query failed
       )))

  (define _transaction-status-type
    (_enum
     '(PQTRANS_IDLE           ; connection idle
       PQTRANS_ACTIVE         ; command in progress
       PQTRANS_INTRANS        ; idle, within transaction block
       PQTRANS_INERROR        ; idle, within failed transaction 
       PQTRANS_UNKNOWN        ; cannot determine status
       )))

  (define _verbosity
    (_enum
     '(PQERRORS_TERSE         ; single-line error messages
       PQERRORS_DEFAULT       ; recommended style
       PQERRORS_VERBOSE       ; all the facts, ma'am
       )))
  
  ;; ============================================================
  ;; connection functions
  
  (define pq_connect
    (get-ffi-obj "PQconnectdb" pq (_fun _bytes -> _pgconn)))
  
  ;; ============================================================
  ;; status functions
  
  (define-values
    (pq_db pq_user pq_pass pq_host pq_port pq_options)
    (apply values
     (map 
      (lambda (fn-name) (get-ffi-obj fn-name pq (_fun _pgconn -> _bytes)))
      '(PQdb PQuser PQpass PQhost PQport PQoptions))))
  
  (define pq_status (get-ffi-obj "PQstatus" pq (_fun _pgconn -> _conn-status-type)))
  
  ;; ============================================================
  ;; query functions
  
  ;; -----
  ;; query execution
  
  (define pq_exec
    (get-ffi-obj "PQexec" pq (_fun _pgconn _bytes -> _pgresult)))
  
  (define pq_result-status
    (get-ffi-obj "PQresultStatus" pq (_fun _pgresult -> _exec-status-type)))
  
  (define pq_res-status
    (get-ffi-obj "PQresStatus" pq (_fun _exec-status-type -> _bytes)))
  
  (define pq_result-error-message
    (get-ffi-obj "PQresultErrorMessage" pq (_fun _pgresult -> _bytes)))
  
  (define pq_clear
    (get-ffi-obj "PQclear" pq (_fun _pgresult -> _void)))
  
  ;; ----
  ;; retrieving values from query results
  
  (define pq_ntuples
    (get-ffi-obj "PQntuples" pq (_fun _pgresult -> _int)))
  
  (define pq_nfields
    (get-ffi-obj "PQnfields" pq (_fun _pgresult -> _int)))
  
  (define pq_fname
    (get-ffi-obj "PQfname" pq (_fun _pgresult _int -> _bytes)))
  
  (define pq_fnumber
    (get-ffi-obj "PQfnumber" pq (_fun _pgresult _bytes -> _int)))
  
  (define pq_ftablecol
    (get-ffi-obj "PQftablecol" pq (_fun _pgresult _int -> _int)))
  
  (define pq_fformat
    (get-ffi-obj "PQfformat" pq (_fun _pgresult _int -> _int)))
  
  (define pq_ftype
    (get-ffi-obj "PQftype" pq (_fun _pgresult _int -> _oid)))

  (define pq_fmod
    (get-ffi-obj "PQfmod" pq (_fun _pgresult _int -> _int)))
  
  (define pq_fsize
    (get-ffi-obj "PQfsize" pq (_fun _pgresult _int -> _int)))
  
  (define pq_binary-tuples
    (get-ffi-obj "PQbinaryTuples" pq (_fun _pgresult -> _bool)))
  
  (define pq_get-value
    (get-ffi-obj "PQgetvalue" pq (_fun _pgresult _int _int -> _bytes)))
  
  (define pq_get-is-null
    (get-ffi-obj "PQgetisnull" pq (_fun _pgresult _int _int -> _bool)))
  
  (define pq_get-length
    (get-ffi-obj "PQgetlength" pq (_fun _pgresult _int _int -> _int)))
  
  
  
  
  #;(define pq_pq-escape-string
    (get-ffi-obj "PQescapeString" 
                 (_fun _pgconn
                       (outstr : (_bytes o (+ (* (length instr) 2) 1)))
                       (instr  : _bytes)
                       _int
                       -> _int
                       -> outstr)))

  
  
  )