FROM tfb/go-base:latest
RUN go get github.com/go-sql-driver/mysql
CMD go run hello_mysql.go -prefork
