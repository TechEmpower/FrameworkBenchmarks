#lang racket/base
(require racket/list
         racket/port
         racket/tcp
         racket/match)

(define RESPONSE
  #"HTTP/1.1 200 OK
Content-Length: 131

<html>\n<head>\n  <title>An Example Page</title>\n</head>\n<body>\n  Hello World, this is a very simple HTML document.\n</body>\n</html>\n\n")
(define END (bytes-length RESPONSE))
(define BUFFER-SIZE 64)
(define BUFFER (make-bytes BUFFER-SIZE))

(struct evt:echo (from to reading writing) #:mutable)

(define (go! port)
  (define l (tcp-listen port 10 #t #f))
  (printf "Ready\n")
  (flush-output)
  (let loop ([evts empty])
    (apply
     sync
     (handle-evt
      (tcp-accept-evt l)
      (match-lambda
       [(list from to)
        (loop (cons (evt:echo from to 0 #f) evts))]))
     (for/list ([e (in-list evts)])
       (match-define (evt:echo from to reading writing) e)
       (match writing
         [#f
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
                           (define new (+ read-k reading))
                           (set-evt:echo-reading! e new)
                           (when (= new 64)
                             (set-evt:echo-writing! e 0)
                             (set-evt:echo-reading! e 0))
                           (loop evts)])))]
         [start
          (handle-evt to
                      (λ (_)
                        (match (write-bytes-avail* RESPONSE to start END)
                          [(or #f 0)
                           (loop evts)]
                          [more
                           (define new (+ start more))
                           (set-evt:echo-writing!
                            e
                            (if (= END new)
                              #f
                              new))
                           (loop evts)])))])))))

(module+ main
  (go! 8000))
