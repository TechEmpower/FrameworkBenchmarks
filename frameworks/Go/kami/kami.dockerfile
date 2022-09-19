FROM docker.io/golang:1.19

COPY ./src /kami
WORKDIR /kami

EXPOSE 8080

CMD GOAMD64=v3 go run ./kami/server.go
