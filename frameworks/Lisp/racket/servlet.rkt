#lang racket/base

(require db
         racket/list
         racket/string
         net/url-structs
         web-server/dispatch
         web-server/http
         web-server/servlet-env
         "./helpers/response-json.rkt")

(define (plaintext req)
  (response
    200 #"OK" (current-seconds) #"text/plain" empty
    (λ (op)
      (write-bytes #"Hello, World!" op))))

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

(define (ensure-integer-is-between-one-and-five-hundreds n)
  (if (number? n)
      (if (< n 1)
        1
        (if (> n 500)
          500
          n))
      1))

(define (extract-number-of-records-to-fetch req)
  (let* ([uri            (request-uri req)]
         [all-parameters (url-query uri)]
         [queries-param  (assoc 'queries all-parameters)]
         [queries        (cdr queries-param)])
    (ensure-integer-is-between-one-and-five-hundreds (string->number queries))))

(define (get-some-random-integers-between-one-and-ten-thousand n)
  (for/list ((i n))
    (add1 (get-a-random-number))))

(define (get-some-random-records n)
  (let ([ids (get-some-random-integers-between-one-and-ten-thousand n)])
    (map (λ (id) (get-a-random-record id)) ids)))

(define (queries req)
  (response/json (get-some-random-records (extract-number-of-records-to-fetch req))))

(define (get-all-fortunes)
  (query-rows pgc "select id, message from fortune"))

(define (get-all-fortunes-plus-one)
  (let* ([records       (get-all-fortunes)]
         [records-p-one (append records '(#(0 "Additional fortune added at request time.")))])
    (sort records-p-one string<? #:key (λ (e)
                                         (vector-ref e 1)))))

(define (fortunes req)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       (head
         (title "Fortunes"))
       (body
         (table
           (tr
             (th "id")
             (th "message"))
           ,@(for/list ([fortune-row (get-all-fortunes-plus-one)])
               `(tr
                  (td ,(format "~v" (vector-ref fortune-row 0)))
                  (td              ,(vector-ref fortune-row 1))))
           )))))

(define (get-and-update-some-random-records n)
  (let* ([random-records (get-some-random-records n)]
         [random-numbers (get-some-random-integers-between-one-and-ten-thousand n)]
         [index -1]
         [updated-records (map (λ (row)
                                 (set! index (add1 index))
                                 (hash 'id           (hash-ref row 'id)
                                       'randomNumber (list-ref random-numbers index)))
                               random-records)]
         [record-list     (map (λ (row)
                                 (list (hash-ref row 'id)
                                       (hash-ref row 'randomNumber)))
                               updated-records)]
         [sql-values      (string-join
                             (map (λ (rec)
                                    (format "(~a, ~a)" (car rec) (car (cdr rec))))
                                  record-list)
                             ", ")]
         [sql-stmt        (string-join
                             `("UPDATE world AS ori SET randomnumber = new.randomnumber FROM (VALUES "
                               ,sql-values
                               ") AS new (id, randomnumber) WHERE ori.id = new.id")
                             "")])
    (query-exec pgc sql-stmt)
    updated-records))

(define (updates req)
  (response/json (get-and-update-some-random-records (extract-number-of-records-to-fetch req))))

(define-values (tfb-dispatch tfb-url)
  (dispatch-rules
    [("plaintext") #:method "get" plaintext]
    [("json")      #:method "get" json]
    [("db")        #:method "get" db]
    [("queries")   #:method "get" queries]
    [("fortunes")  #:method "get" fortunes]
    [("updates")   #:method "get" updates]
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
