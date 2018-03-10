FROM tfb/go-base:latest
RUN go get github.com/lib/pq
CMD go run hello_postgres.go
