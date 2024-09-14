FROM golang:1.22-alpine

ENV GO111MODULE=on \
    CGO_ENABLED=0  \
    GOAMD64=v3     \
    GOARCH="amd64" \
    GOOS=linux

WORKDIR /go/goravel

COPY ./src/fiber /go/goravel

RUN go mod tidy

RUN go generate -x ./templates

RUN go build -ldflags '-s -w --extldflags "-static"' -o /go/goravel/main

EXPOSE 8080

CMD /go/goravel/main
