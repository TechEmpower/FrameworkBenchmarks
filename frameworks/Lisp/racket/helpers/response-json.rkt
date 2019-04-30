#lang racket/base

(require json
         racket/list
         web-server/http)

(define (response/json output)
  (response
    200 #"Okay" (current-seconds) #"application/json; charset=utf-8" empty
    (λ (out)
      (write-bytes (jsexpr->bytes output) out))))

(provide (all-defined-out))
