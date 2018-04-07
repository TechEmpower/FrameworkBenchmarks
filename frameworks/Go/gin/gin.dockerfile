FROM golang:1.10.1

ADD ./ /gin
WORKDIR /gin

RUN mkdir bin
ENV GOPATH /gin
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/gin-gonic/gin
RUN go get github.com/go-sql-driver/mysql

CMD go run hello.go
