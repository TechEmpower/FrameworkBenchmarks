#lang racket/base
(require racket/tcp)

(define RESPONSE #"HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\n!")
(define END (bytes-length RESPONSE))
(define BUFFER-SIZE 64)
(define BUFFER (make-bytes BUFFER-SIZE))

(struct evt:echo (from to writing) #:mutable)

(define (go! port)
  (define l (tcp-listen port 10 #t #f))
  (printf "Ready\n")
  (flush-output)
  (let loop ([evts null])
    (apply
     sync
     (handle-evt
      (tcp-accept-evt l)
      (lambda (l)
        (define from (car l))
        (define to (cadr l))
        (loop (cons (evt:echo from to #f) evts))))
     (for/list ([e (in-list evts)])
       (define from (evt:echo-from e))
       (define to (evt:echo-to e))
       (define write-start (evt:echo-writing e))
       (cond
         [write-start
          (handle-evt to
                      (λ (_)
                        (define write-k
                          (write-bytes-avail* RESPONSE to write-start END))
                        (cond
                          [(or (not write-k) (zero? write-k))
                           (loop evts)]
                          [else
                           (define new (+ write-start write-k))
                           (set-evt:echo-writing!
                            e
                            (if (= END new)
                              #f
                              new))
                           (loop evts)])))]
         [else
          (handle-evt from
                      (λ (_)
                        (define read-k
                          (read-bytes-avail!* BUFFER from 0 BUFFER-SIZE))
                        (cond
                          [(eof-object? read-k)
                           (close-input-port from)
                           (close-output-port to)
                           (loop (remq e evts))]
                          [else
                           (set-evt:echo-writing! e 0)
                           (loop evts)])))])))))

(module+ main
  (go! 8000))
