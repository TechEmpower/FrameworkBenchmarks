FROM docker.io/golang:1.19

ENV GO111MODULE on
WORKDIR /evio

COPY ./src /evio

RUN go mod download

RUN go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
