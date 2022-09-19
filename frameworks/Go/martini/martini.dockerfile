FROM docker.io/golang:1.19-alpine

WORKDIR /home
COPY . .

RUN go mod download

EXPOSE 8080

CMD go run .
