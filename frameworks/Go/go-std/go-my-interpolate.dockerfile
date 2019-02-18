FROM golang:1.11.5

ADD ./src/mysql/ /go-std
WORKDIR /go-std

RUN mkdir bin
ENV GOPATH /go-std
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/go-sql-driver/mysql
RUN go build -o hello_mysql .
CMD ./hello_mysql
