FROM golang:1.14

ADD ./ /kami
WORKDIR /kami

RUN mkdir bin
ENV GOPATH /kami
ENV PATH ${GOPATH}/bin:${PATH}

RUN go get github.com/go-sql-driver/mysql
RUN go get github.com/guregu/kami

EXPOSE 8080

CMD go run src/kami/server.go
