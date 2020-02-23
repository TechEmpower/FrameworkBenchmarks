#lang racket/base

(require db
         json
         threading
         web-server/dispatch
         web-server/http)

;; db ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define max-db-conns 1024)
(define db-conn-pool
  (connection-pool
   #:max-connections max-db-conns
   #:max-idle-connections max-db-conns
   (λ ()
     (postgresql-connect #:database "hello_world"
                         #:user "benchmarkdbuser"
                         #:password "benchmarkdbpass"
                         #:server "tfb-database"))))

(define db-conn-pool-sema
  (make-semaphore max-db-conns))

(define current-db-conn
  (make-parameter #f))

(define (call-with-db-conn f)
  (call-with-semaphore db-conn-pool-sema
    (lambda ()
      (define conn #f)
      (dynamic-wind
        (lambda ()
          (set! conn (connection-pool-lease db-conn-pool)))
        (lambda ()
          (parameterize ([current-db-conn conn])
            (f)))
        (lambda ()
          (disconnect conn))))))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (response/json e)
  (response/output
   #:mime-type #"application/json; charset=utf-8"
   (lambda (out)
     (write-json e out))))


;; world ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct world (id n)
  #:transparent)

(define select-one-world
  (virtual-statement
   (lambda (_dbsystem)
     "SELECT id, randomnumber FROM world WHERE id = $1")))

(define update-one-world
  (virtual-statement
   (lambda (_dbsystem)
     "UPDATE world SET randomnumber = $2 WHERE id = $1")))

(define (random-world-id)
  (random 1 10001))

(define (random-world-ids n)
  (for/list ([_ (in-range n)])
    (random-world-id)))

(define (worlds-ref id)
  (for/first ([(id n) (in-query (current-db-conn) select-one-world id)])
    (world id n)))

(define (worlds-ref/random n)
  (define conn (current-db-conn))
  (for*/list ([id (in-list (random-world-ids n))]
              [(id n) (in-query conn select-one-world id)])
    (world id n)))

(define (worlds-update! rs)
  (define conn (current-db-conn))
  (for ([r (in-list rs)])
    (query-exec conn update-one-world (world-id r) (world-n r))))

(define (world->hash r)
  (hash 'id (world-id r)
        'randomNumber (world-n r)))


;; fortune ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct fortune (id message)
  #:transparent)

(define select-fortunes
  (virtual-statement
   (lambda (_dbsystem)
     "SELECT id, message FROM fortune")))

(define (all-fortunes)
  (define fortunes
    (cons
     (fortune 0 "Additional fortune added at request time.")
     (for/list ([(id message) (in-query (current-db-conn) select-fortunes)])
       (fortune id message))))

  (sort fortunes string<? #:key fortune-message))

(define (fortune->table-row f)
  `(tr
    (td ,(number->string (fortune-id f)))
    (td ,(fortune-message f))))


;; web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-queries req)
  (or
   (and~> (request-bindings/raw req)
          (bindings-assq #"queries" _)
          (binding:form-value)
          (bytes->string/utf-8)
          (string->number)
          (min 500)
          (max 1))
   1))

(define-values (dispatch _url)
  (dispatch-rules
   [("plaintext")
    (lambda (_req)
      (response/output
       #:mime-type #"text/plain"
       (lambda (out)
         (display "Hello, World!" out))))]

   [("json")
    (lambda (_req)
      (response/json
       (hasheq 'message "Hello, World!")))]

   [("db")
    (lambda (_req)
      (define world-id (random-world-id))
      (define world
        (call-with-db-conn
         (lambda ()
           (worlds-ref world-id))))

      (response/json
       (world->hash world)))]

   [("fortunes")
    (lambda (_req)
      (define fortunes
        (call-with-db-conn all-fortunes))

      (response/xexpr
       #:preamble #"<!DOCTYPE html>"
       `(html
         (head
          (title "Fortunes"))
         (body
          (table
           (tr
            (th "id")
            (th "message"))
           ,@(map fortune->table-row fortunes))))))]

   [("queries")
    (lambda (req)
      (define n (parse-queries req))
      (define worlds
        (call-with-db-conn
         (lambda ()
           (worlds-ref/random n))))

      (response/json
       (map world->hash worlds)))]

   [("updates")
    (lambda (req)
      (define worlds
        (call-with-db-conn
         (lambda ()
           (define n (parse-queries req))
           (define worlds (worlds-ref/random n))
           (define worlds*
             (for/list ([r (in-list worlds)]
                        [n (in-list (random-world-ids n))])
               (struct-copy world r [n n])))

           (begin0 worlds*
             (worlds-update! worlds*)))))

      (response/json
       (map world->hash worlds)))]))

(module+ main
  (require web-server/servlet-dispatch
           web-server/web-server)

  (define stop
    (serve
     #:dispatch (dispatch/servlet dispatch)
     #:listen-ip "0.0.0.0"
     #:port 8080))

  (with-handlers ([exn:break?
                   (lambda (_e)
                     (stop))])
    (sync/enable-break never-evt)))
