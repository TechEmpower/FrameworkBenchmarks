FROM docker.io/golang:1.19

ADD ./src /webgo
WORKDIR /webgo

EXPOSE 8080

CMD go run ./hello
