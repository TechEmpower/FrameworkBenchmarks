FROM golang:1.23-alpine as builder

WORKDIR /fiber

COPY ./src /fiber

RUN go mod download && \
    go generate -x ./templates && \
    GOAMD64=v3 go build -ldflags="-s -w" -o app .

FROM alpine:latest

WORKDIR /fiber

COPY --from=builder /fiber/app .

EXPOSE 8080

CMD ./app -prefork
