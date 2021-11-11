#lang racket/base

(require db
         json
         racket/fasl
         racket/port
         racket/serialize
         racket/unix-socket-tcp-unit
         redis
         threading
         web-server/dispatch
         web-server/http
         web-server/http/response
         xml)

;; db ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *db*
  (virtual-connection
   (connection-pool
    (lambda ()
      (postgresql-connect #:database "hello_world"
                          #:user "benchmarkdbuser"
                          #:password "benchmarkdbpass"
                          #:server "tfb-database")))))


;; cache ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *redis*
  (make-redis-pool
   #:pool-size 32))

(define (deserialize* bs)
  (deserialize (fasl->s-exp bs)))

(define (serialize* v)
  (s-exp->fasl (serialize v)))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (response/bytes bs
                        #:code [code 200]
                        #:headers [headers null]
                        #:mime-type [mime-type #"text/plain"])
  (define len:bs (string->bytes/utf-8 (number->string (bytes-length bs))))
  (response/output
   #:code code
   #:mime-type mime-type
   #:headers (cons (make-header #"Content-Length" len:bs) headers)
   (lambda (out)
     (write-bytes bs out))))

(define (response/json e)
  (response/bytes
   #:mime-type #"application/json; charset=utf-8"
   (jsexpr->bytes e)))

(define (response/xexpr xe)
  (response/bytes
   #:mime-type #"text/html; charset=utf-8"
   (call-with-output-bytes
    (lambda (out)
      (write-bytes #"<!DOCTYPE html>" out)
      (write-xexpr xe out)))))

;; world ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(serializable-struct world (id n)
  #:transparent)

(define select-one-world
  (virtual-statement "SELECT id, randomnumber FROM world WHERE id = $1"))

(define update-one-world
  (virtual-statement "UPDATE world SET randomnumber = $2 WHERE id = $1"))

(define (random-world-id)
  (random 1 10001))

(define (random-world-ids n)
  (for/list ([_ (in-range n)])
    (random-world-id)))

(define (worlds-ref id)
  (for/first ([(id n) (in-query *db* select-one-world id)])
    (world id n)))

(define (worlds-ref/random n)
  (for*/list ([id (in-list (random-world-ids n))]
              [(id n) (in-query *db* select-one-world id)])
    (world id n)))

(define (worlds-update! rs)
  (for ([r (in-list rs)])
    (query-exec *db* update-one-world (world-id r) (world-n r))))

(define (world->hash r)
  (hash 'id (world-id r)
        'randomNumber (world-n r)))


;; fortune ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct fortune (id message)
  #:transparent)

(define select-fortunes
  (virtual-statement "SELECT id, message FROM fortune"))

(define (all-fortunes)
  (define fortunes
    (cons
     (fortune 0 "Additional fortune added at request time.")
     (for/list ([(id message) (in-query *db* select-fortunes)])
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
      (response/bytes #"Hello, World!"))]

   [("json")
    (lambda (_req)
      (response/json
       (hasheq 'message "Hello, World!")))]

   [("db")
    (lambda (_req)
      (define world-id (random-world-id))
      (define world (worlds-ref world-id))
      (response/json
       (world->hash world)))]

   [("fortunes")
    (lambda (_req)
      (response/xexpr
       `(html
         (head
          (title "Fortunes"))
         (body
          (table
           (tr
            (th "id")
            (th "message"))
           ,@(map fortune->table-row (all-fortunes)))))))]

   [("queries")
    (lambda (req)
      (define n (parse-queries req))
      (define worlds (worlds-ref/random n))
      (response/json
       (map world->hash worlds)))]

   [("cached")
    (let ([local-cache (make-hasheqv)])
      (lambda (req)
        (define n (parse-queries req))
        (define worlds
          (hash-ref! local-cache n (lambda ()
                                     (call-with-redis-client *redis*
                                       (lambda (rc)
                                         (define k (format "worlds:~a" n))
                                         (cond
                                           [(redis-bytes-get rc k)
                                            => deserialize*]

                                           [else
                                            (define worlds (worlds-ref/random n))
                                            (begin0 worlds
                                              (redis-bytes-set! rc k (serialize* worlds)))]))))))

        (response/json
         (map world->hash worlds))))]

   [("updates")
    (lambda (req)
      (define n (parse-queries req))
      (define worlds
        (for/list ([r (in-list (worlds-ref/random n))]
                   [n (in-list (random-world-ids n))])
          (struct-copy world r [n n])))

      (worlds-update! worlds)
      (response/json
       (map world->hash worlds)))]))

(module+ main
  (require racket/async-channel
           racket/cmdline
           racket/format
           web-server/http/response
           web-server/safety-limits
           web-server/web-server)

  (define port
    (command-line
     #:args (port)
     (string->number port)))

  (define (app c req)
    (output-response c (dispatch req)))

  (define ch (make-async-channel))
  (define stop
    (serve
     #:dispatch app
     #:listen-ip "127.0.0.1"
     #:port port
     #:tcp@ (make-unix-socket-tcp@ (format "~a.sock" port))
     #:confirmation-channel ch
     #:safety-limits (make-safety-limits
                      #:max-waiting 4096
                      #:request-read-timeout 16
                      #:response-timeout 16
                      #:response-send-timeout 16)))

  (define ready-or-exn (sync ch))
  (when (exn:fail? ready-or-exn)
    (raise ready-or-exn))

  (call-with-output-file (build-path (~a port ".ready"))
    (lambda (out)
      (displayln "ready" out)))

  (with-handlers ([exn:break?
                   (lambda (_e)
                     (stop))])
    (sync/enable-break never-evt)))
