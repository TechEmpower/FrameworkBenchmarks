FROM golang as build-env

WORKDIR /src/
ADD ./src /src/

RUN go build -o app

FROM gcr.io/distroless/base:debug

ENV GIN_MODE=release

COPY --from=build-env /src/app /app
ENTRYPOINT ["/app"]