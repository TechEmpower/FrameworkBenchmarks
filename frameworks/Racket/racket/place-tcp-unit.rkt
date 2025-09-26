#lang racket/base

(require net/tcp-sig
         (prefix-in tcp: racket/tcp)
         racket/unit)

(provide
 make-place-tcp@)

(struct place-tcp-listener ())

(define (make-place-tcp@ accept-ch)
  (unit
    (import)
    (export tcp^)

    (define (tcp-abandon-port p)
      (tcp:tcp-abandon-port p))

    (define (tcp-accept _l)
      (apply values (channel-get accept-ch)))

    (define (tcp-accept/enable-break _l)
      (apply values (sync/enable-break accept-ch)))

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
      (place-tcp-listener))

    (define (tcp-listener? l)
      (place-tcp-listener? l))))
