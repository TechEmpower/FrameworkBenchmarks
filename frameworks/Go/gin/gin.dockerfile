FROM golang:1.18

ENV GO111MODULE=off

ADD ./ /gin
COPY ./templates /templates
WORKDIR /gin

RUN mkdir bin
ENV GOPATH /gin
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/gin-gonic/gin
RUN go get github.com/go-sql-driver/mysql

RUN go build -o hello hello.go

EXPOSE 8080

CMD ./hello
