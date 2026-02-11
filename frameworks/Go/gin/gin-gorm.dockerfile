FROM docker.io/golang:1.24.2 as build-env

WORKDIR /src/
COPY ./gin-gorm /src/

RUN GOAMD64=v3 go build -o app .

FROM gcr.io/distroless/base:debug

ENV GIN_MODE=release

COPY --from=build-env /src/app /app
ENTRYPOINT ["/app"]
