FROM techempower/go-base:0.1

RUN go get github.com/go-sql-driver/mysql
CMD go run hello_mysql.go
