FROM techempower/go-base:0.1

RUN go get github.com/lib/pq
CMD go run hello_postgres.go
