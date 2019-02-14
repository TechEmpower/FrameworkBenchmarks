FROM golang:1.11.5

ADD ./ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/go-sql-driver/mysql
RUN go build -o hello_mysql hello_mysql.go
CMD ./hello_mysql -prefork
