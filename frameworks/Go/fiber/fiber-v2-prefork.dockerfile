FROM golang:1.25.6-alpine as builder

WORKDIR /fiber

COPY ./fiber-v2 /fiber

RUN go mod download && \
    go install github.com/valyala/quicktemplate/qtc@latest && \
    qtc && \
    GOAMD64=v3 go build -ldflags="-s -w" -o app .

FROM alpine:latest

WORKDIR /fiber

COPY --from=builder /fiber/app .

EXPOSE 8080

CMD ./app -prefork
