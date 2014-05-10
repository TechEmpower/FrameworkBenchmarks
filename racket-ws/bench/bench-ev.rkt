#lang racket/base
(require racket/tcp)

(define RESPONSE #"HTTP/1.1 200 OK\r\nContent-Length: 1\r\n\r\n!")
(define END (bytes-length RESPONSE))
(define BUFFER-SIZE 64)
(define const-eof (λ (x) eof))

(struct mevt ([evt #:mutable])
        #:property prop:evt (λ (e) (mevt-evt e)))

(define (go! port)
  (define BUFFER (make-bytes BUFFER-SIZE))
  (define EVTS null)
  (define l (tcp-listen port 10 #t #f))  
  (define accept-evt
    (handle-evt
     (tcp-accept-evt l)
     (lambda (l)
       (define from (car l))
       (define to (cadr l))
       (define (write-f to)
         (write-bytes-avail* RESPONSE to 0 END)
         (set-mevt-evt! e read-evt))
       (define write-evt
         (handle-evt to write-f))
       (define (read-f from)
         (define read-k
           ;; XXX This drops performance from about 100k to 88k, but
           ;; is necessary because of crashes running the benchmarks
           (with-handlers ([exn:fail? const-eof])
             (read-bytes-avail!* BUFFER from 0 BUFFER-SIZE)))
         (cond
           [(eof-object? read-k)
            (close-input-port from)
            (close-output-port to)
            (set! EVTS (remq e EVTS))]
           [else
            (set-mevt-evt! e write-evt)]))
       (define read-evt
         (handle-evt from read-f))
       (define e
         (mevt read-evt))
       (set! EVTS (cons e EVTS)))))
  (printf "Ready\n")
  (flush-output)
  (let loop ()
    (apply sync accept-evt EVTS)
    (loop)))

(module+ main
  (go! 8000))
