#lang racket/base

(require racket/cmdline
         racket/match
         racket/place
         racket/tcp
         (prefix-in app: "app.rkt")
         "place-tcp-unit.rkt")

(define (start-place)
  (place ch
    (define accept-ch (make-channel))
    (define tcp (make-place-tcp@ accept-ch))
    (define stop #f)

    (let loop ([pid #f])
      (match (sync ch)
        [`(stop)
         (stop)
         (eprintf "place ~a stopped~n" pid)]
        [`(init ,pid ,host ,port)
         (set! stop (app:start host port tcp))
         (eprintf "place ~a ready~n" pid)
         (loop pid)]
        [`(accept ,in ,out)
         (channel-put accept-ch (list in out))
         (loop pid)]))))

(module+ main
  (define-values (host port parallelism)
    (let ([host "127.0.0.1"]
          [port 8000]
          [parallelism (processor-count)])
      (command-line
       #:once-each
       [("--host" "-H") HOST "the host to listen on" (set! host HOST)]
       [("--port" "-p") PORT "the port to bind to"
                        (define port-num (string->number PORT))
                        (unless (and port-num (>= port-num 0) (< port-num 65536))
                          (eprintf "error: PORT must be a number between 0 and 65535, inclusive~n")
                          (exit 1))
                        (set! port port-num)]
       [("--parallelism" "-P") PARALLELISM "the number of parallel places to run"
                               (define n-places (string->number PARALLELISM))
                               (unless (and n-places (positive? n-places))
                                 (eprintf "error: PARALLELISM must be a positive number~n")
                                 (exit 1))
                               (set! parallelism n-places)]
       #:args []
       (values host port parallelism))))

  (define places
    (for/list ([pid (in-range parallelism)])
      (define p (start-place))
      (begin0 p
        (place-channel-put p `(init ,pid ,host ,port)))))
  (define (stop-places)
    (for ([ch (in-list places)])
      (place-channel-put ch `(stop)))
    (for-each place-wait places))
  (define place-fail-evt
    (apply choice-evt (map place-dead-evt places)))

  (define backlog
    (* parallelism 65 1024))
  (define listener
    (tcp-listen port backlog #t host))
  (define stop-ch (make-channel))
  (define listener-thd
    (thread
     (lambda ()
       (define places* (list->vector places))
       (define num-places (vector-length places*))
       (define stop-evt (choice-evt stop-ch place-fail-evt))
       (let loop ([idx 0])
         (sync
          (handle-evt
           listener
           (lambda (_)
             (define-values (in out)
               (tcp-accept listener))
             (place-channel-put (vector-ref places* idx) `(accept ,in, out))
             (tcp-abandon-port out)
             (tcp-abandon-port in)
             (loop (modulo (add1 idx) num-places))))
          (handle-evt
           stop-evt
           (lambda (_)
             (stop-places)
             (tcp-close listener))))))))
  (define (stop)
    (channel-put stop-ch #t)
    (thread-wait listener-thd))

  (with-handlers ([exn:break?
                   (lambda (_e)
                     (stop))])
    (sync/enable-break never-evt listener-thd)))
