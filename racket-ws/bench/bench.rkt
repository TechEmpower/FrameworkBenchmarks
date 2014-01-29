#lang racket/base
(require web-server/servlet-env
         web-server/dispatch
         web-server/http
         json
         db)

(define DEPLOY? #t)

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
    (cond
      [DEPLOY?
       (virtual-connection
        (connection-pool
         (λ ()
           (mysql-connect #:user "benchmarkdbuser"
                          #:password "benchmarkdbpass"
                          #:database "hello_world"
                          #:server db-host))))]
      [else
       (define c (sqlite3-connect #:database 'memory))
       (query-exec c "create table World ( id int, randomNumber int )")
       (for ([i (in-range (add1 10000))])
         (query-exec c "insert into World values ( $1, $1 )" i))
       c]))

  (define (db-one)
    (define id (add1 (random 10000)))
    (define randomNumber
      (query-value c "select randomNumber from World where id = ?" id))
    (hash 'id id 'randomNumber randomNumber))

  (define (page/db req)
    (response/json
     (db-one)))

  (define (clamp lo x hi)
    (cond
      [(x . < . lo) lo]
      [(hi . < . x) hi]
      [else          x]))

  (define (page/dbs req is)
    (define maybe-num (string->number is))
    (define maybe-i (or maybe-num 0))
    (define i (clamp 1 maybe-i 500))
    (response/json
     (for/list ([j (in-range i)])
       (db-one))))

  (define-values (main-dispatch main-url)
    (dispatch-rules
     [("json")
      page/json]
     [("db")
      page/db]
     [("dbs" (string-arg))
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
