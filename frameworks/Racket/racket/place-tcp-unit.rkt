#lang racket/base

(require net/tcp-sig
         (prefix-in tcp: racket/tcp)
         racket/unit)

(provide
 make-place-tcp@)

(struct place-tcp-listener (sema ch)
  #:property prop:evt (lambda (self)
                        (handle-evt
                         (place-tcp-listener-sema self)
                         (lambda (_) self))))

(define (make-place-tcp@ accept-ch)
  (unit
    (import)
    (export tcp^)

    (define (tcp-abandon-port p)
      (tcp:tcp-abandon-port p))

    (define (tcp-accept l)
      (apply values (channel-get (place-tcp-listener-ch l))))

    (define (tcp-accept/enable-break l)
      (apply values (sync/enable-break (place-tcp-listener-ch l))))

    (define (tcp-accept-ready? _l)
      (error 'tcp-accept-ready? "not supported"))

    (define (tcp-addresses _p [port-numbers? #f])
      (if port-numbers?
          (values "127.0.0.1" 1 "127.0.0.1" 0)
          (values "127.0.0.1" "127.0.0.1")))

    (define (tcp-close _l)
      (void))

    (define (tcp-connect _hostname
                         _port-no
                         [_local-hostname #f]
                         [_local-port-no #f])
      (error 'tcp-connect "not supported"))

    (define (tcp-connect/enable-break _hostname
                                      _port-no
                                      [_local-hostname #f]
                                      [_local-port-no #f])
      (error 'tcp-connect/enable-break "not supported"))

    (define (tcp-listen _port-no
                        [_backlog 4]
                        [_reuse? #f]
                        [_hostname #f])
      (define sema (make-semaphore))
      (define ch (make-channel))
      (thread
       (lambda ()
         (let loop ()
           (define data (channel-get accept-ch))
           (semaphore-post sema)
           (channel-put ch data)
           (loop))))

      (place-tcp-listener sema ch))

    (define (tcp-listener? l)
      (place-tcp-listener? l))))
