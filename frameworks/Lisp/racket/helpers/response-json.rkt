#lang racket/base

(require json
         racket/list
         web-server/http)

(define APPLICATION/JSON-MIME-TYPE #"application/json; charset=utf-8")

(define (response/json
         output
         #:code [code 200]
         #:message [message #"Okay"]
         #:seconds [seconds (current-seconds)]
         #:mime-type [mime-type APPLICATION/JSON-MIME-TYPE]
         #:cookies [cooks empty]
         #:headers [hdrs empty]
         #:preamble [preamble #""])
  (response
    code message seconds mime-type
    ; rfc2109 also recommends some cache-control stuff here for cookies
    (append hdrs (map cookie->header cooks))
    (λ (out)
      (write-bytes preamble out)
      (write-bytes (jsexpr->bytes output) out))))

(provide (all-defined-out))
