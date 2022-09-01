# build layer
FROM golang:1.18-alpine as builder

WORKDIR /gin
ENV GO111MODULE=on

COPY ./ /gin

RUN apk update \
    apk add --no-cache git \
    go mod download

# Start Contrast Additions
COPY contrast-go contrast-go
COPY contrast_security.yaml /etc/contrast/contrast_security.yaml

ENV CONTRAST__ASSESS__ENABLE=false
ENV CONTRAST__PROTECT__ENABLE=true

ENV CONTRAST__AGENT__SERVICE__BYPASS=true

RUN chmod 777 ./contrast-go
# End Contrast Additions

RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 \
    ./contrast-go build -tags=jsoniter -ldflags="-s -w" -o server /gin/*.go

RUN apk --no-cache add --update ca-certificates

# release layer
FROM scratch as release

COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt
COPY --from=builder /gin/server /bin/server
COPY --from=builder /gin/templates/fortune.html /templates/fortune.html

EXPOSE 8080

ENTRYPOINT ["/bin/server"]
