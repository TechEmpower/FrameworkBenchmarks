# build layer
FROM docker.io/golang:1.19-alpine as builder

ADD ./src/chi /chi
WORKDIR /chi

RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 GOAMD64=v3 \
    go build -ldflags="-w -s" -o server

RUN apk --no-cache add --update ca-certificates

# release layer
FROM scratch

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
COPY --from=builder /chi/server /bin/server

EXPOSE 8080

ENTRYPOINT ["/bin/server"]
