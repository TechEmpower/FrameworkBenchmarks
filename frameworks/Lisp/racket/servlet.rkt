#lang racket/base

(require db
         json
         racket/list
         web-server/dispatch
         web-server/http
         web-server/servlet-env)

(define (plaintext req)
  (response/xexpr
   `(html (body "Hello world!"))))

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

(define (json req)
  (response/json (hash 'message "Hello, World!")))

(define (get-a-random-number)
  (+ 1 (random 10000)))

(define pgc
    (postgresql-connect #:database "hello_world"
                        #:user "benchmarkdbuser"
                        #:password "benchmarkdbpass"
                        #:server "tfb-database"))

(define (get-a-random-record id)
  (hash 'id id 'randomNumber (query-value pgc "select randomnumber from world where id = $1" id)))

(define (db req)
  (response/json (get-a-random-record (get-a-random-number))))

(define-values (tfb-dispatch tfb-url)
  (dispatch-rules
   [("plaintext") #:method "get" plaintext]
   [("json")      #:method "get" json]
   [("db")        #:method "get" db]
   [else plaintext]))

(define (start request)
  (tfb-dispatch request))

(serve/servlet start
               #:command-line? #t
               #:launch-browser? #f
               #:listen-ip #f
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:stateless? #t)
