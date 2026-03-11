FROM golang:1.26-alpine AS builder

RUN apk add --no-cache git

WORKDIR /kruda
COPY ./src /kruda

RUN GOAMD64=v3 go build -ldflags="-s -w" -gcflags="-B" -trimpath -o app .

FROM alpine:3.20
RUN apk --no-cache add ca-certificates tzdata
WORKDIR /kruda
COPY --from=builder /kruda/app .

EXPOSE 8080
ENV GOGC=500 GOMEMLIMIT=512MiB
CMD ["./app"]
