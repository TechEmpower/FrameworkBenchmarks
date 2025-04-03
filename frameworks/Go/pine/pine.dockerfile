FROM docker.io/golang:1.24.2

COPY ./src /pine
WORKDIR /pine

RUN go mod download

EXPOSE 8080

CMD go run .
