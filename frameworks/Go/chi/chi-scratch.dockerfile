# build layer
FROM golang:1.14-alpine as builder

ADD ./src/chi /chi
WORKDIR /chi
ENV GO111MODULE=on

RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -ldflags="-w -s" -o server

RUN apk --no-cache add --update ca-certificates

# release layer
FROM scratch

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
COPY --from=builder /chi/server /bin/server

EXPOSE 8080

ENTRYPOINT ["/bin/server"]
