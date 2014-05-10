#lang racket/base
(module+ main
  (require racket/tcp)

  (define PORT 8000)

  (define RESPONSE #"HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\n!")
  (define END (bytes-length RESPONSE))
  (define BUFFER-SIZE 64)
  (define BUFFER (make-bytes BUFFER-SIZE))
  (define const-eof (λ (x) eof))

  (define K 0)

  (define EVTS null)
  (define l (tcp-listen PORT 10 #t #f))
  (define (accept-f l)
    (set! K (add1 K))
    (printf "Conns: ~a\n" K) (flush-output)
    (define from (car l))
    (define to (cadr l))
    (define (read-f from)
      (define read-k
        ;; XXX This drops performance from about 132k/s to 120k/s,
        ;; but is necessary because of crashing the benchmarks
        (with-handlers ([exn:fail? const-eof])
          (read-bytes-avail!* BUFFER from 0 BUFFER-SIZE)))
      (cond
        [(eof-object? read-k)
         (close-input-port from)
         (close-output-port to)
         (set! EVTS (remq e EVTS))]
        [else
         (write-bytes-avail* RESPONSE to 0 END)]))
    (define e
      (handle-evt from read-f))
    (set! EVTS (cons e EVTS)))
  (define accept-evt
    (handle-evt (tcp-accept-evt l) accept-f))
  (printf "Ready\n")
  (flush-output)
  (let loop ()
    (apply sync accept-evt EVTS)
    (loop)))
