FROM docker.io/golang:1.24.2

WORKDIR /evio

COPY ./src /evio

RUN GOAMD64=v3 go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app
