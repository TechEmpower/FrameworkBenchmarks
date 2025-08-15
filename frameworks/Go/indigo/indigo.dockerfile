FROM golang:1.25-alpine3.22 as builder

WORKDIR /indigo

COPY ./src /indigo

RUN go mod download && \
    go generate -x ./templates && \
    GOAMD64=v3 go build -ldflags="-s -w" -o app .

FROM alpine:3.22

WORKDIR /indigo

COPY --from=builder /indigo/app .

EXPOSE 8080

CMD ./app
