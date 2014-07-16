#lang racket/base
(require web-server/servlet-env
         web-server/dispatch
         web-server/http
         json
         db)

(define (response/json x)
  (response/output
   #:mime-type #"application/json"
   (λ (out)
     (write-json x out))))

(define (page/json req)
  (response/json
   (hasheq 'message "Hello, World!")))

(define (go! db-host)
  (define c
    (virtual-connection
     (connection-pool
      (λ ()
        (mysql-connect #:user "benchmarkdbuser"
                       #:password "benchmarkdbpass"
                       #:database "hello_world"
                       #:server db-host)))))

  (define (db-one)
    (define random-id (add1 (random 10000)))
    (query-list c "select * from World where randomNumber = $1" random-id))

  (define (page/db req)
    (response/json
     (db-one)))

  (define (page/dbs req i)
    (response/json
     (for/list ([j (in-range i)])
       (db-one))))

  (define-values (main-dispatch main-url)
    (dispatch-rules
     [("json")
      page/json]
     [("db")
      page/db]
     [("dbs" (integer-arg))
      page/dbs]))

  (serve/servlet
   main-dispatch
   #:port 8000
   #:listen-ip #f
   #:command-line? #t
   #:servlet-regexp #rx""
   #:servlet-path "/"))

(module+ main
  (require racket/cmdline)
  (command-line #:program "bench"
                #:args (db-host-s)
                (go! db-host-s)))
