# builder layer
FROM docker.io/golang:1.20-alpine as builder

WORKDIR /fiber

COPY ./src /fiber

RUN apk update && \
    apk add --no-cache git && \
    apk --no-cache add --update ca-certificates && \
    go mod download && \
    go generate -x ./templates && \
    CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -ldflags="-s -w" -o server .

# Before copying the Go binary directly to the final image,
# add them to the intermdediate upx image
FROM gruebel/upx:latest as upx
COPY --from=builder /fiber/server /server.orig

# Compress the binary and copy it to final image
RUN upx --best --lzma -o /server /server.orig

# release layer
FROM scratch as release

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
#COPY --from=builder /fiber/server /bin/server
COPY --from=upx /server .
# COPY --from=builder /fiber/templates/fortune.html /templates/fortune.html

EXPOSE 8080

ENTRYPOINT ["/server"]
