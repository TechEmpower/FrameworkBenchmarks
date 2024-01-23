# build layer
FROM docker.io/golang:1.19-alpine as builder

WORKDIR /gin
COPY ./gin-std /gin

RUN apk update \
    apk add --no-cache git \
    go mod download

RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 GOAMD64=v3 \
    go build -tags=jsoniter -ldflags="-s -w" -o server .

RUN apk --no-cache add --update ca-certificates

# release layer
FROM scratch as release

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
COPY --from=builder /gin/server /bin/server
COPY --from=builder /gin/templates/fortune.html /templates/fortune.html

EXPOSE 8080

ENTRYPOINT ["/bin/server"]
