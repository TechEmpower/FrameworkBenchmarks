FROM golang:latest

WORKDIR /go/src/app

# ENV GO111MODULE=on
COPY vendor ./vendor
# COPY go.mod .
# COPY go.sum .
# RUN go mod download
COPY src/. .

RUN go build -ldflags="-s -w" -o app .

CMD ./app
