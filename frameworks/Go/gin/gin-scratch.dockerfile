# build layer
FROM golang:1.14-alpine as builder

WORKDIR /gin
ENV GO111MODULE=on

COPY ./ /gin

RUN apk update \
    apk add --no-cache git \
    go mod download

RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    go build -tags=jsoniter -ldflags="-s -w" -o server /gin/*.go

RUN apk --no-cache add --update ca-certificates

# release layer
FROM scratch as release

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
COPY --from=builder /gin/server /bin/server
COPY --from=builder /gin/templates/fortune.html /templates/fortune.html

EXPOSE 8080

ENTRYPOINT ["/bin/server"]
