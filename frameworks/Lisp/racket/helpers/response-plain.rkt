#lang racket/base

(require racket/list
         web-server/http)

(define TEXT/PLAIN-MIME-TYPE #"text/plain; charset=utf-8")

(define (response/plain
         output
         #:code [code 200]
         #:message [message #"Okay"]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type TEXT/PLAIN-MIME-TYPE]
         #:cookies [cooks empty]
         #:headers [hdrs empty]
         #:preamble [preamble #""])
  (response
    code message seconds mime-type
    ; rfc2109 also recommends some cache-control stuff here for cookies
    (append hdrs (map cookie->header cooks))
    (λ (out)
      (write-bytes preamble out)
      (write-bytes (string->bytes/utf-8 output) out))))

(provide (all-defined-out))
