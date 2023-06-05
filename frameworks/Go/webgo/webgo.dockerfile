FROM docker.io/golang:1.19

ADD ./src /webgo
WORKDIR /webgo

RUN go mod download

EXPOSE 8080

CMD GOAMD64=v3 go run ./hello
