FROM golang:1.13

ENV GO111MODULE on
WORKDIR /fiber

COPY ./src /fiber

RUN go mod download

RUN go build -o server server.go

CMD ./server -prefork
