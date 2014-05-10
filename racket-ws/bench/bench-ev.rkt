#lang racket/base
(require racket/list
         racket/port
         racket/tcp
         racket/match)

(define RESPONSE
  #"HTTP/1.1 200 OK
Date: Mon, 23 May 2005 22:38:34 GMT
Server: Apache/1.3.3.7 (Unix) (Red-Hat/Linux)
Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT
Content-Type: text/html; charset=UTF-8
Content-Length: 131
Accept-Ranges: bytes
Connection: close

<html>
<head>
  <title>An Example Page</title>
</head>
<body>
  Hello World, this is a very simple HTML document.
</body>
</html>

")
(define END (bytes-length RESPONSE))

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
        (loop (cons (evt:echo from to 'waiting-for-return #f) evts))]))
     (for/list ([e (in-list evts)])
       (match-define (evt:echo from to reading writing) e)
       (match writing
         [#f
          (handle-evt from
                      (λ (_)
                        (define res (read-byte from))
                        (flush-output)
                        (cond
                          [(eof-object? res)
                           (close-input-port from)
                           (close-output-port to)
                           (loop (remq e evts))]
                          [(and (eq? reading 'waiting-for-newline-two)
                                (= res (char->integer #\newline)))
                           (set-evt:echo-writing! e 0)
                           (loop evts)]
                          [(and (eq? reading 'waiting-for-return-two)
                                (= res (char->integer #\return)))
                           (set-evt:echo-reading! e 'waiting-for-newline-two)
                           (loop evts)]
                          [(and (eq? reading 'waiting-for-newline)
                                (= res (char->integer #\newline)))
                           (set-evt:echo-reading! e 'waiting-for-return-two)
                           (loop evts)]
                          [(and (eq? reading 'waiting-for-return)
                                (= res (char->integer #\return)))
                           (set-evt:echo-reading! e 'waiting-for-newline)
                           (loop evts)]
                          [else
                           (set-evt:echo-reading! e 'waiting-for-return)
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
