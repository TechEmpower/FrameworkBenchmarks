FROM docker.io/golang:1.24.2

WORKDIR /ronykit

COPY ./src /ronykit

RUN go mod download

RUN GOAMD64=v3 go build -ldflags="-s -w" -o app .

EXPOSE 8080

CMD ./app -prefork
